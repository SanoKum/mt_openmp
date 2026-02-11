#!/usr/bin/env python3
"""
Generate input file with doubled pitchwise (circumferential) mesh.
IM: 64 -> 128, pitchwise spacings: 63 -> 127 values.

Keep the same wall stretching (13 values each side), add more uniform
points in the middle (37 -> 101 uniform cells at 10.0).
"""

import sys

input_file = sys.argv[1] if len(sys.argv) > 1 else "../test_cases/two-stg-LP-ST+steam.dat"
output_file = sys.argv[2] if len(sys.argv) > 2 else "../test_cases/two-stg-LP-ST+steam-im128.dat"

with open(input_file, 'r') as f:
    lines = f.readlines()

out_lines = []
i = 0
while i < len(lines):
    line = lines[i]

    # Find "GRID POINT NUMBERS  IM, KM" header
    if 'GRID POINT NUMBERS' in line:
        out_lines.append(line)
        i += 1
        # Replace IM=64 with IM=128, keep KM=64
        out_lines.append('  128   64\n')
        i += 1  # skip original "   64   64"

        # Next is "RELATIVE PITCHWISE GRID SPACINGS" header
        out_lines.append(lines[i])  # header line
        i += 1

        # Read existing 63 values (8 lines: 7 lines of 8 values + 1 line of 7)
        old_values = []
        while len(old_values) < 63:
            vals = lines[i].split()
            old_values.extend([float(v) for v in vals])
            i += 1

        # Original pattern: 13 stretching + 37 uniform(10.0) + 13 stretching = 63
        stretch_start = old_values[:13]   # 1.0 -> 10.0
        stretch_end = old_values[-13:]    # 10.0 -> 1.0

        # New: 13 stretching + 101 uniform + 13 stretching = 127
        new_values = stretch_start + [10.0] * 101 + stretch_end
        assert len(new_values) == 127, f"Expected 127, got {len(new_values)}"

        # Write 127 values, 8 per line (15 full lines + 1 line with 7)
        for j in range(0, len(new_values), 8):
            chunk = new_values[j:j+8]
            formatted = ''.join(f'{v:10.5f}' for v in chunk)
            out_lines.append(formatted + '\n')
    else:
        out_lines.append(line)
        i += 1

with open(output_file, 'w') as f:
    f.writelines(out_lines)

print(f"Created {output_file}")
print(f"  IM: 64 -> 128")
print(f"  Pitchwise spacings: 63 -> 127 values")
print(f"  Total lines: {len(out_lines)}")
