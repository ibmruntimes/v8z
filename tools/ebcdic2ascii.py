#!/usr/bin/env python

import re, sys, optparse

# This is a utility for converting literals in V8 source code from
# EBCDIC encoding to ASCII.  
OPEN_PAREN          = "(\()"
CLOSE_PAREN         = "(\))"
NEWLINE             = "(\\n)"
COMMA               = "(,)"
OUTSTREAM_OP        = "(<<)"
UNICODE_PRE         = "u8"
PRINT_FUNCTIONS     = "(PrintError|printf|PrintF|VFPrintF|SPrintF|sprintf|vfprintf|feprintf|FePrintF|open)"
USTR_MACRO          = "(USTR)"         
STRING              = '(".*?(?<!\\\\)")'
CHAR                = "('.{1,2}')"
HEX_CHAR            = "('\\\\x[0-9A-Fa-f]{1,2}')"
IGNORE_STRING       = "#\s*include|#\s*pragma|\s*//|extern\s+\"C\""
HEX_ENCODED_STRING  = r'(?:\\x[0-9A-Fa-f]{1,2})+'
CONCAT              = "(##)"
STRINGIFY           = "(#{1}\w+)"
WHITESPACE          = "(\s*)"
SPLIT_TOKEN_LIST    = [ PRINT_FUNCTIONS, USTR_MACRO, COMMA, NEWLINE, UNICODE_PRE, OPEN_PAREN, CLOSE_PAREN, OUTSTREAM_OP, STRING, CHAR, HEX_CHAR,  CONCAT, STRINGIFY]

EBCDIC_PRAGMA_START  = re.compile(r'\s*#pragma\s+convert\s*\(\s*\"IBM-1047\"\s*\)|\s*#pragma\s+convert\s*\(\s*\"ibm-1047\"\s*\)')
EBCDIC_PRAGMA_END    = re.compile(r"\s*#pragma\s+convert\s*\(\s*pop\s*\)")
MULTILINE_COMMENT_START  = re.compile(r"^\s*/\*")
MULTILINE_COMMENT_END    = re.compile(r".*\*/\s*")


#ignore lines starting with
IGNORE_RE = re.compile(IGNORE_STRING)

#C-string literals in the source
DEFINE_RE             = re.compile(r'#pragma|#import|#error|#define|#undef|#endif|#if|#ifdef|#else|#elseif|#elif')
OUTSTREAM_OP_RE       = re.compile(OUTSTREAM_OP)
CLOSE_PAREN_RE        = re.compile(CLOSE_PAREN)
STRING_RE             = re.compile(STRING)
CHAR_RE               = re.compile(CHAR)
NEWLINE_RE            = re.compile(NEWLINE)
HEX_ENCODED_STRING_RE = re.compile(HEX_ENCODED_STRING) 
PRINT_FUNCTIONS_RE    = re.compile(PRINT_FUNCTIONS)
USTR_MACRO_RE         = re.compile(USTR_MACRO)
STRINGIFY_RE          = re.compile(STRINGIFY)
OPEN_PAREN_RE         = re.compile(OPEN_PAREN)
CLOSE_PAREN_RE        = re.compile(CLOSE_PAREN)
SPLIT_RE              = re.compile(('|').join(SPLIT_TOKEN_LIST))

#TOKENIZER FOR string literal
ESCAPE_RE    = re.compile(r'\\n|\\t|\\v|\\r|\\f|\\a|\\b|\\\'|\\\"|\\\\|\\0')
HEX_RE       = re.compile(r"(\\x[0-9A-Fa-f]{1,2})")
OCTAL_RE     = re.compile(r"(\\[0-7]{1,3})")
UNICODE_RE1  = re.compile(r"(\\u\[0-9A-Fa-f]{1,4})")
UNICODE_RE2  = re.compile(r"(\\U\[0-9A-Fa-f]{1,8})")
ENCODING_RE  = re.compile(r"(\\x[0-9A-Fa-f]{1,2} |\\\\[0-7]{1,3} |\\\\u\[0-9A-Fa-f]{1,4} |\\\\U\[0-9A-Fa-f]{1,8})")

PRINTF_RE=re.compile('%{1}\s*[-+#0]*\s*[0-9]*[.]*[0-9]*[hljztL]*[iduoxXffFeEgGaAcspn]+')


#CONVERSION TABLES
ESCAPE_LIT = {"\\n":'\n', "\\t":'\t', "\\v":'\v', "\\r":'\r',  "\\f":'\f',
              "\\a":'\a', "\\b":'\b', "\\'":'\'', "\\\"":'\"', "\\\\":'\\',
              "\\0":'\0'}

EBCDIC_TO_ASCII = [
0x00, 0x01, 0x02, 0x03, 0xff, 0x09, 0xff, 0x7f, 0xff, 0xff, 0xff, 0x0b, 0x0c, 
0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0xff, 0x0a, 0x08, 0xff, 0x18, 0x19, 
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x1c, 0xff, 0xff, 0x0a, 0x17, 
0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0x05, 0x06, 0x07, 0xff, 0xff, 0x16, 0xff, 
0xff, 0xff, 0xff, 0x04, 0x14, 0x15, 0xff, 0xff, 0xff, 0xff, 0xff, 0x1a, 0x20, 
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x2e, 0x3c, 0x28, 
0x2b, 0x7c, 0x26, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x21, 
0x24, 0x2a, 0x29, 0x3b, 0x5e, 0x2d, 0x2f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0x2c, 0x25, 0x5f, 0x3e, 0x3f, 0xff, 0xff, 0xff, 0xff, 0xff, 
0xff, 0xff, 0xff, 0xff, 0x60, 0x3a, 0x23, 0x40, 0x27, 0x3d, 0x22, 0xff, 0x61, 
0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0xff, 0xff, 0xff, 0xff, 0xff, 
0xff, 0xff, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0xff, 0xff, 
0xff, 0xff, 0xff, 0xff, 0xff, 0x7e, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 
0x7a, 0xff, 0xff, 0xff, 0x5b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x5d, 0xff, 0xff, 0x7b, 0x41, 0x42, 
0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
0x7d, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0xff, 0xff, 0xff, 
0xff, 0xff, 0xff, 0x5c, 0xff, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 
0x37, 0x38, 0x39, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]

def EncodeInEBCDIC(literal):
   convert = "";
   for byte in literal:
          hex_lit = str(hex(ord(byte)))
          convert = convert + '\\x' + hex_lit[2:4]
   return convert 

def EncodeInASCII(literal):
   convert = "";
   for byte in literal:
          ascii_lit  = str(hex(EBCDIC_TO_ASCII[ord(byte)]))
          convert = convert + '\\x' + ascii_lit[2:4]
   return convert 

def ConvertMacroArgs(token):
   if DEFINE_RE.match(token):
      return token
   if STRINGIFY_RE.match(token): 
      token = token.strip()
      return " USTR("+token+")"
   return token

def ConvertTokens(tokens):
    if not HEX_RE.search(tokens):
       return EncodeInASCII(tokens)
    else:
       return tokens

def EncodeEscapeSeq(literal):
    return EncodeInASCII(ESCAPE_LIT[literal.group(0)])

def EncodeChars(literal):
    return EncodeInASCII(literal.group(0))

def EncodePrintF(literal):
    return EncodeInEBCDIC(literal.group(0))

def main():
  parser = optparse.OptionParser()
  parser.set_usage("""ebcdic2ascii.py [options] input.cc output.cc 
   input.cc: File to be converted 
   output.cc: Converted File.""")
  parser.add_option("-u", action="store_true", dest="unicode_support", default = False, help="convert strings using u8 prefix")
  parser.add_option("--skip_print", action="store_true", dest="skip_print_strings", default = False, help="skip strings going to snprtinf,printf,output stream")

  (options, args) = parser.parse_args()
                    
  Source          = open(args[0], "rt")
  Target          = open(args[1], "at+")
  
  unicode_encode             = options.unicode_support;
  skip_print_strings         = options.skip_print_strings; 
  
  ebcdic_encoding = False
  convert_start = False
  convert_end = False
  
  multiline_comment = False
  comment_start = False
  comment_end = False
  
  skip_line = False 
  
  #Main loop which identifies and encodes literals with hex escape sequences
  for line in Source:
    comment_start = MULTILINE_COMMENT_START.match(line)
    convert_start = EBCDIC_PRAGMA_START.match(line)
    multiline_comment = (multiline_comment or comment_start)\
                        and (not comment_end)
    ebcdic_encoding = (ebcdic_encoding or convert_start)\
                      and (not convert_end)
    skip_line = IGNORE_RE.match(line)\
                or multiline_comment or ebcdic_encoding 
    
    if not skip_line:
       tokens_of_interest = re.split(SPLIT_RE, line)
       tokens_of_interest = filter(None, tokens_of_interest)
       #Mark strings inside functions and between outstream_op which we do not want to be modified 
       delimiters = []
       delete = False
       open_paren = 0
       start_index = 0
       stop_index  = 0
       
       for index in range(len(tokens_of_interest)):
           token = tokens_of_interest[index]
           if USTR_MACRO_RE.match(token):
              start_index = index
              delete = True
           if skip_print_strings and  PRINT_FUNCTIONS_RE.match(token):
              start_index = index
              delete = True
           if delete and OPEN_PAREN_RE.match(token):
              open_paren = open_paren + 1;
           if delete and (CLOSE_PAREN_RE.match(token) or NEWLINE_RE.match(token)):
              open_paren = open_paren - 1; 
              if open_paren == 0:
                 stop_index = index
                 delimiters.append((start_index, stop_index))
       
       index = 0;
       newline = ""
       delimiters.reverse()
       start_index = -1;
       stop_index  = -1;
       ostream_string = False
       #print tokens_of_interest
       #print delimiters
       
       while index < (len(tokens_of_interest)):
           token = tokens_of_interest[index];
           
           if USTR_MACRO_RE.match(token) or (skip_print_strings and PRINT_FUNCTIONS_RE.match(token)):
              if len(delimiters) > 0:
                 (start_index, stop_index) = delimiters.pop();
              token = token
              newline = newline + token
              index = index +1
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
       line = newline;     
    
    comment_end = MULTILINE_COMMENT_END.match(line)
    convert_end = EBCDIC_PRAGMA_END.match(line)
    line = line 
    Target.write(line) 
  
  Source.close()
  Target.close()
  return 0

if __name__ == "__main__":
   sys.exit(main())
