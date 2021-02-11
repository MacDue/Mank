#!/usr/bin/python3
import sys
from os.path import dirname, join

# Very lazy preprocessor script

def read_lines(path):
  with open(path, 'r') as f:
    return f.readlines()

if __name__ == "__main__":
  prog = sys.argv[1]

  prog_source = read_lines(prog)

  for idx, line in enumerate(prog_source):
    line = line.strip()
    if line.startswith('#'):
      pp_command = line[1:].split()
      if pp_command[0] == "!copy":
        copy_path = pp_command[1]
        base_path = dirname(prog)
        copy_path = join(base_path, copy_path)
        copy_source = read_lines(copy_path)
        prog_source[idx] = ''.join(copy_source)

  for line in prog_source:
    print(line)
