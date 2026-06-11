import difflib
import glob
import re

# Regex to extract the number right after 'opt-'
filename_number_pattern = re.compile(r"^.*opt-(\d+)")
file_list = []

# Gather files and filter/sort by the integer value
for filename in glob.glob("_optstages/opt-*.lcc"):
    number_match = filename_number_pattern.match(filename)
    if number_match:
        num = int(number_match.group(1))
        file_list.append((num, filename))

# Sort by the number in the filename to ensure correct sequential order
file_list.sort(key=lambda x: x[0])

# Read files and generate diffs between each unique sequential pair of
# files.
# Given:
#   opt-1056-untouched.lcc
#   opt-1057-icmb.lcc
#   opt-1058-sroa.lcc
#
# Diff 1056x1057 and 1057x1058
for i in range(len(file_list) - 1):
    lhs_filename = file_list[i][1]
    rhs_filename = file_list[i + 1][1]

    with open(lhs_filename, "r", encoding="utf-8", errors="ignore") as lhs_file, open(
        rhs_filename, "r", encoding="utf-8", errors="ignore"
    ) as rhs_file:
        lhs_lines = lhs_file.read().splitlines()
        rhs_lines = rhs_file.read().splitlines()

    # Setup unified diff `diff -u`
    diff = difflib.unified_diff(
        lhs_lines,
        rhs_lines,
        fromfile=lhs_filename,
        tofile=rhs_filename,
        lineterm=""
    )

    # Print the diff
    for line in diff:
        print(line)
