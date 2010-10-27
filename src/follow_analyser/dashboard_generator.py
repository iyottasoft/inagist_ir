#!/usr/bin/env python2.6

import sys
import os, glob
from string import lower

def main():
  if (len(sys.argv) != 3):
    print "Usage: %s data_dir user_name" % (sys.argv[0])
    return -1

  data_dir = sys.argv[1]
  user_name = sys.argv[2]
  path = data_dir + '/' + user_name + '/'
  file_name_pattern = 'trends.*'
  kfiles = glob.glob(os.path.join(path, file_name_pattern))
  kfiles.sort(key=lambda x: os.path.getmtime(x))

  pf = open('/tmp/testjsons/dashboard/' + user_name + '.html', 'w')
  pf.write('<html>\n')
  pf.write('<head>\n')
  pf.write('<h1 align="center">%s Dashboard</h1>\n' % (sys.argv[2]))
  pf.write('</head>\n')
  pf.write('<body>\n')
  pf.write('<table>\n')
  pf.write('<tr>\n')
  for kfile in kfiles:
    pf.write('<td width="20%" valign="top">\n')
    fp = open(kfile, 'r')
    lines = fp.readlines()
    for line in lines:
      line = line.strip()
      tokens = line.split(" | ")
      if (len(tokens) != 3):
        print "error"
        break
      pf.write('<a href="http://inagist.com/trends/%s">' % (lower(tokens[2].replace(' ', '_'))))
      pf.write('%s</a> (%s, %s)' % (tokens[2], tokens[1], tokens[0]))
      pf.write('<br/>')
    fp.close()
    pf.write('</td>')
  pf.write('</tr>\n')
  pf.write('</table>\n')
  pf.write('</html>\n')
  pf.write('</body>\n')
  pf.close()

  return 0

if __name__ == "__main__":
  main()
