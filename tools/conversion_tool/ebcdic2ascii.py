#!/usr/bin/env python

import re, sys, optparse, read_files, os
from ebcdic2ascii_encoder import *

# takes in a file list and parameters for unicode encoding and skipping print strings
# intended for use by other python programs, not for scripts for commandline
def open_files(file_list, unicode_encode=False, skip_print_strings=False, include_paths=[]):

    # error-check: need exactly two file paths in file_list
    if len(file_list) != 2:
        print("ERROR: Source and target file path required.")
        return 1

    convert_to_ascii(file_list, unicode_encode, skip_print_strings, include_paths)

    # resets the global blacklist contained in read_files.py for header files
    read_files.reset_blacklist()
    return 0

# makes the delimiters given tokens of interest
def make_delimiters(tokens_of_interest, skip_print_strings):
    delimiters = []
    delete = False
    continued_string = False
    open_paren = 0
    start_index = 0
    stop_index  = 0

    for index in range(len(tokens_of_interest)):
        token = tokens_of_interest[index]
        if USTR_MACRO_RE.match(token):
            start_index = index
            delete = True
        if skip_print_strings and PRINT_FUNCTIONS_RE.match(token):
            start_index = index
            delete = True
        if delete and OPEN_PAREN_RE.match(token):
            open_paren = open_paren + 1;
        if delete and (CLOSE_PAREN_RE.match(token) or NEWLINE_RE.match(token)):
            open_paren = open_paren - 1;
            if open_paren == 0:
                stop_index = index
                delimiters.append((start_index, stop_index))

    return delimiters

# takes the tokens and determines the new line that will replace the old
def determine_new_line(tokens_of_interest, delimiters, skip_print_strings, unicode_encode):
    index = 0;
    newline = ""
    delimiters.reverse()
    start_index = -1;
    stop_index  = -1;
    ostream_string = False
    while index < (len(tokens_of_interest)):
        token = tokens_of_interest[index];

        if USTR_MACRO_RE.match(token) or (skip_print_strings and PRINT_FUNCTIONS_RE.match(token)):
            if len(delimiters) > 0:
                (start_index, stop_index) = delimiters.pop();
            token = token
            newline = newline + token
            index = index + 1
            continue

        if index >= start_index and index <= stop_index:
            token = token
            newline = newline + token
            index = index + 1
            continue

        if skip_print_strings and OUTSTREAM_OP_RE.match(token):
            if not ostream_string:
                ostream_string = True;
            else:
                ostream_string = False;

        if NEWLINE_RE.match(token):
            if ostream_string:
                ostream_string = False;

        if STRING_RE.match(token):
            if not ostream_string:
                literal = token
                if unicode_encode:
                    if not HEX_ENCODED_STRING_RE.match(literal):
                        unicode_literal = "u8" + literal
                        token = unicode_literal
                else:
                    encoded_literal = literal[1:len(literal)-1]
                    encoded_literal = re.sub(ESCAPE_RE, EncodeEscapeSeq,encoded_literal)
                    encoded_literal = re.sub(PRINTF_RE, EncodePrintF, encoded_literal)
                    token_list = re.split(HEX_RE, encoded_literal)
                    encoded_literal = reduce(lambda x,y: x+y, map(ConvertTokens, token_list))
                    encoded_literal = "\"" + encoded_literal + "\""
                    token = encoded_literal

        if CHAR_RE.match(token):
            if not ostream_string:
                char = token[1:len(token)-1]
                encoded_char = ''
                escape_seq = ESCAPE_RE.match(char)
            if escape_seq:
                encoded_char = EncodeEscapeSeq(escape_seq)
            else:
                encoded_char = ConvertTokens(char)
            char = "'" + char + "'"
            encoded_char = "'" + encoded_char + "'"
            token =  encoded_char

        if STRINGIFY_RE.match(token):
            converted_token = ConvertMacroArgs(token)
            token = converted_token

        newline = newline + token
        index = index + 1;

    return newline

# given an include statement line, call the fuction that recursively deals
# with header files then return the name of the new temporary header file
def find_target_header(line, filenames, include_paths):
    target_header = 0
    quotes = True

    # check if it's a "" or a <> include statement
    include_file = FILE_QUOTES_RE.match(line)
    if include_file is None:
        quotes = False
        include_file = FILE_BRACKETS_RE.match(line).group(1)
    else:
        include_file = include_file.group(1)

    # check if its an absolute path, as those take priority
    a = ABSOLUTE_RE.match(include_file)
    if a is not None:
        target_header = read_files.recursive_headers(include_file,
          include_file, include_paths)
    else:
        # get only the end of the file - the filename
        include_end = FILE_END_RE.search(include_file)
        if include_end is not None:
            include_end = include_end.group(2)
        else:
            include_end = include_file

        # if the filename is in the include_paths_names providied by the .h file
        # search for the path provided in the include_paths array
        for path in include_paths:
            if include_end == path.split('/')[-1]:
                full_path = path
                end_path = include_file
                target_header = read_files.recursive_headers(full_path, end_path, include_paths)
        if target_header == 0:
            # otherwise, search using the name itself
            file_beginning = FILE_END_RE.match(filenames[0])
            if file_beginning is not None:
                target_header = read_files.recursive_headers(file_beginning.group(1) \
                  + "/" + include_file, include_file, include_paths)
            else:
                target_header = read_files.recursive_headers(include_file, \
                  include_file, include_paths)

    # if all else fails, it is a standard library include,
    # rewrite it as the original
    if target_header == 1 or target_header == 0:
        target_header = include_file

    return target_header, quotes

# main function to convert from ebcdic to ascii
def convert_to_ascii(filenames, unicode_encode, skip_print_strings, \
  include_paths):
    Source          = open(filenames[0], "rt")
    Target          = open(filenames[1], "at+")

    # define USTR written at the top of the temp c/c++ file
    if (filenames[0][-3:] == ".cc" or filenames[0][-4:] == ".cpp" or filenames[0][-2:] == ".c"):
        Target.write('#define USTR(x) u8##x\n')

    # flags to determine exactly what the line of code in the source contains
    ebcdic_encoding = False
    convert_start = False
    convert_end = False

    multiline_comment = False
    comment_start = False
    comment_end = False

    skip_line = False
    include_line = False
    combine_lines = False
    prev_line = None

    # main loop which identifies and encodes literals with hex escape sequences
    for line in Source:

        # logic for line continuations; appends the lines
        if prev_line is not None:
            line = prev_line + line
            prev_line = None
        backslash = BACKSLASH_RE.match(line)
        if backslash:
            backslash = backslash.group(1)
            prev_line = backslash
            continue

        # check if the line is a comment
        comment_start = MULTILINE_COMMENT_START.match(line)
        multiline_comment = (multiline_comment or comment_start)\
         and (not comment_end)

        # check if conversion is requested
        convert_start = EBCDIC_PRAGMA_START.match(line)
        ebcdic_encoding = (ebcdic_encoding or convert_start)\
         and (not convert_end)

        # check if the line should be ignored
        skip_line = IGNORE_RE.match(line)\
         or multiline_comment or ebcdic_encoding
        include_line = INCLUDE_RE.match(line)

        # if it isn't to be skipped
        if not skip_line and not include_line:
            tokens_of_interest = re.split(SPLIT_RE, line)
            tokens_of_interest = filter(None, tokens_of_interest)

            # mark strings inside functions and between outstream_op
            # which we do not want to be modified
            delimiters = make_delimiters(tokens_of_interest, skip_print_strings)

            #check what the current line is, determine what to make the new line
            line = determine_new_line(tokens_of_interest, delimiters, \
              skip_print_strings, unicode_encode);

        # if the line is an #include statement
        if include_line and not skip_line:

            # find the name of the new temp header file
            (target_header, quotes) = find_target_header(line, filenames, \
              include_paths)

            # write the name of the target header where the source header once was
            if quotes:
                Target.write('#include "' + target_header + '"\n')
            else:
                Target.write('#include <' + target_header + '>\n')

        else:
            comment_end = MULTILINE_COMMENT_END.match(line)
            convert_end = EBCDIC_PRAGMA_END.match(line)
            line = line
            Target.write(line)

    Source.close()
    Target.close()

# parses the arguments given from command line or os process in order to determine
# the filenames, unicode encoding, and skip print strings options
def parse_arguments():

    # parse the command line arguments, looking for potential flags -u and --skip_print
    # along with two file paths, input and output
    parser = optparse.OptionParser()
    parser.set_usage("""ebcdic2ascii.py [options] input.cc output.cc include_paths
    input.cc: File to be converted
    output.cc: Converted File.""")
    parser.add_option("-u", action="store_true", dest="unicode_support", default = False, help="convert strings using u8 prefix")
    parser.add_option("--skip_print", action="store_true", dest="skip_print_strings", default = False, help="skip strings going to snprtinf,printf,output stream")
    parser.add_option("-H", action="store", dest="headers", default="", help="provide a file that contains all the dependencies")
    parser.add_option("-I", action="store", dest="ignore", default="")

    (options, args) = parser.parse_args()

    # error-check: two arguments from command-line expected
    if len(args) < 2:
        print("ERROR: Source and target file path required.")
        return 1

    unicode_encode             = options.unicode_support;
    skip_print_strings         = options.skip_print_strings;

    p = None
    if options.ignore != "":
        PRIVATE_MATCH = re.compile("(.+)/node")
        p = PRIVATE_MATCH.match(options.ignore)
        if p is not None:
            p = p.group(1)

    # go through the header file provided and determine the file path and file
    # name for every header path provided
    path = os.environ.get('NODE')
    includes = []
    if options.headers is not None and os.path.isfile(options.headers):
        header_file = open(options.headers, 'rt')
        for line in header_file:
            multiline = MULTIPLE_HEADERS.match(line)
            while (multiline is not None):
                curr = multiline.group(1).strip()
                if p is not None:
                    if p not in curr and curr != "\\" and ".node-gyp" not in curr:
                        includes.append(curr)
                else:
                    absolute_match = ABSOLUTE_RE.match(curr)
                    if absolute_match is None:
                        includes.append(curr)
                multiline = MULTIPLE_HEADERS.match(multiline.group(2))

    convert_to_ascii(args, unicode_encode, skip_print_strings, includes)

    # resets the global blacklist contained in read_files.py for header files
    read_files.reset_blacklist()
    return 0

if __name__ == "__main__":
    sys.exit(parse_arguments())
