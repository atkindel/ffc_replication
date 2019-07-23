#!/usr/local/bin/python3
#
# code_analysis.py
# Lexical analysis over user submissions.
# First, ensure dependencies in requirements.txt are installed.
#
# Author: Alex Kindel
# Last major edits: 15 March 2018
# Compatibility edits: 23 July 2019

import os
import csv
import subprocess
import methodologist.methodologist as md
from collections import Counter, defaultdict

from rpy2 import robjects
from rpy2.robjects.packages import importr

# Set directory paths
# SUBMISSIONS_CONTENTS is previously populated by prep_data.sh
BASE_DIR = os.path.abspath(os.path.join(os.getcwd(), os.pardir))
DATA_DIR = os.path.join(BASE_DIR, "data")
SUBMISSIONS_CONTENTS = os.path.join(DATA_DIR, "submissions")

# List of valid code extensions
CODE_EXTS = ["py", "r", "do", "sas", "m", "f"]


# Identify file type by extension
def extension(path):
    return path.split('.')[-1]

# Extract code from RMarkdown
def preprocess_Rmd(code_path):
    # Purl file and return new file path
    try:
        knitr = importr("knitr")
        robjects.r('purl("{}", output="{}")'.format(code_path, code_path[:-2]))
        return code_path[:-2]
    except Exception as e:
        print(code_path)
        raise(e)

# Extract code from Juypter
def preprocess_jupyter(code_path):
    nbconvert = subprocess.run("jupyter nbconvert --to script {}".format(code_path.replace(" ", "\\ ")), shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    return code_path.replace(".ipynb", ".py")

# Read and lexify a script
def parse_code(code_path):
    code = None
    with open(fpath, 'r') as cf:
        code = cf.read()
    functions = md.lexify(code)
    return functions


# Find all code files in submissions
# Also, resolves literate scripts to code only
def read_code_files():
    exts = list()
    code_data = list()
    created = list()

    # Read all script files
    try:
        for root, dirs, files in os.walk(SUBMISSIONS_CONTENTS):
            for fn in files:
                # Identify file path
                full_path = os.path.join(root, fn)
                print(full_path)
                fext = extension(fn).lower()

                # Identify user
                user = root.split('-')[0].split('/')[-1].rstrip()

                # Identify code type
                code_file = None
                if fext in CODE_EXTS:
                    code_file = full_path
                elif fext == "ipynb":
                    # Resolve Jupyter
                    code_file = preprocess_jupyter(full_path)
                    fext = 'py'
                    created.append(code_file)
                elif fext == "rmd":
                    # Resolve RMarkdown
                    code_file = preprocess_Rmd(full_path)
                    fext = 'r'
                    created.append(code_file)
                else:
                    continue  # Skip file if not code
                exts.append(fext)

                # Parse script
                code = md.lexify(code_file)

                # Save data
                cdat = dict()
                cdat["user"] = user
                cdat['filename'] = code_file
                cdat['extension'] = fext
                cdat['code'] = code
                code_data.append(cdat)

                # Alert
                print(user)
                print(fn)
                print()

    finally:
        # Clean up all temporary files
        for tmpf in created:
            os.remove(tmpf)

    # Compute extension counts
    extensions = Counter(exts)

    return code_data, extensions


# Write code data to CSV
def write_code_data(code_data, outfile):
    header = code_data[0].keys()
    with open(os.path.join(DATA_DIR, outfile), "w") as dat:
        write = csv.DictWriter(dat, header)
        write.writeheader()
        for cd in code_data:
            write.writerow({k: v for k, v in cd.items()})


if __name__ == "__main__":
    code_data, extensions = read_code_files()
    write_code_data(code_data, "ffc_code.csv")
    print(extensions)
