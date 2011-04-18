#!/usr/bin/env python2.6

import sys

def main():

  if len(sys.argv) != 3:
    print "Usage: %s <input_text_file> <output_html_file>" % sys.argv[0]
    return -1

  input_text_file = sys.argv[1]
  output_html_file = sys.argv[2]

  f = open(input_text_file, 'r')
  lines = f.readlines()
  f.close()

  f = open(output_html_file, 'w')
  f.write('<html>\n<head>\n')
  f.write('<title>Inagist Classification Demo</title>\n')
  f.write('<h1>Inagist Classification Demo</h1>\n')
  f.write('</head>\n<body>\n')
  f.write('<table>\n')
  expected_class_name = ''
  for line in lines:
    line = line.strip()
    tokens = line.split('|')
    if len(tokens) < 3:
      print "error"
      break
    if (expected_class_name != tokens[0]):
      expected_class_name = tokens[0]
      f.write('<tr><td><hr/>')
      f.write(expected_class_name)
      f.write('<hr/></td></tr>')
    output_class_name = tokens[1]
    if (output_class_name == "XX" or 
        output_class_name == "UU" or
        output_class_name == "RR"):
      f.write('<tr><td bgcolor=#FFFFFF>')
    elif (expected_class_name != output_class_name):
      f.write('<tr><td bgcolor=#FF0000>\n')
    else:
      f.write('<tr><td bgcolor=#07B133>\n')
    f.write(line)
    f.write('</td></tr>\n')
  f.write('</table>\n')
  f.write('</body>\n</html>')
  f.close()

  return 0

if __name__ == "__main__":
  main()
