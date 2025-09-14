#!/usr/bin/env python3

import argparse
import os

_CONFIG_FILES_PREFIX = 'ac_config_files="$ac_config_files'

def main():


    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, help='Input file path')
    parser.add_argument('--output', required=True, help='Output file path')
    parser.add_argument('input_files', nargs='+', help='Expected input files')
    args = parser.parse_args()

    wanted = set(args.input_files)

    with open(args.input, 'r') as infile, open(args.output, 'w') as outfile:
        for line in infile:
            if line.startswith(_CONFIG_FILES_PREFIX):
                config_files = line.removeprefix(_CONFIG_FILES_PREFIX).rstrip('"\n\r').split(" ")

                line_builder = [_CONFIG_FILES_PREFIX]
                for config_file in config_files:
                    parts = config_file.split(":")
                    if len(parts) == 2 and parts[1] in wanted:
                        line_builder.extend([" ", config_file])

                line_builder.append('"\n')
                line = " ".join(line_builder)
            elif line.startswith('ac_aux_files="'):
                # Remove all deps
                line = 'ac_aux_files=""\n'

            outfile.write(line)

    os.chmod(args.output, os.stat(args.input).st_mode)

if __name__ == '__main__':
    main()
