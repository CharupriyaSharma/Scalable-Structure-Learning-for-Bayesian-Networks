import sys

correct1 = int(sys.argv[1])
flipped1 = int(sys.argv[2])
missing1 = int(sys.argv[3])
additional1 = int(sys.argv[4])
total = correct1+missing1
correct2 = int(sys.argv[5])
flipped2 = int(sys.argv[6])
missing2 = int(sys.argv[7])
additional2 = int(sys.argv[8])
missing1 -= flipped1
additional1 -= flipped1
missing2 -= flipped2
additional2 -= flipped2

b1 = correct1 / total
b2 = correct1 / total + flipped1 / total
b3 = correct1 / total + flipped1 / total + missing1 / total
b4 = correct1 / total + flipped1 / total + missing1 / total + additional1 / total
c1 = correct2 / total
c2 = correct2 / total + flipped2 / total
c3 = correct2 / total + flipped2 / total + missing2 / total
c4 = correct2 / total + flipped2 / total + missing2 / total + additional2 / total

b1 *= 17.0
b2 *= 17.0
b3 *= 17.0
b4 *= 17.0
t1 = b1 * 0.5
t2 = b1 + (b2-b1) * 0.5
t3 = b2 + (b3-b2) * 0.5
t4 = b3 + (b4-b3) * 0.5
c1 *= 17.0
c2 *= 17.0
c3 *= 17.0
c4 *= 17.0
t5 = c1 * 0.5
t6 = c1 + (c2-c1) * 0.5
t7 = c2 + (c3-c2) * 0.5
t8 = c3 + (c4-c3) * 0.5

print("\\begin{tikzpicture}[xscale=0.25]")
print("\\node[anchor=east] (text) at (-0.1,0.25){\\tiny BN-MARS};")
print("\draw[fill=green!30] (0,0) rectangle (", b1, ",0.5); \\node[anchor=center] (text) at (", t1, ",0.25){\\tiny A};")
print(" \draw[fill=blue!30] (", b1, ",0) rectangle (", b2, ",0.5); \\node[anchor=center] (text) at (", t2, ",0.25){\\tiny F};")
print(" \draw[fill=red!30] (", b2, ",0) rectangle (", b3, ",0.5); \\node[anchor=center] (text) at (", t3, ",0.25){\\tiny M};")
print(" \draw[fill=orange!30] (", b3, ",0) rectangle (", b4, ",0.5);  \\node[anchor=center] (text) at (", t4, ",0.25){\\tiny E};")
print(" \\node[anchor=east] (text) at (-1.5,-0.15){\\texttt{",sys.argv[9],"}};")
print(" \\begin{scope}[yshift=-8mm]")
print("  \\node[anchor=east] (text) at (-0.1,0.25){\\tiny BN-RF-MARS};")
print("  \draw[fill=green!30] (0,0) rectangle (", c1, ",0.5); \\node[anchor=center] (text) at (", t5, ",0.25){\\tiny A};")
print("  \draw[fill=blue!30] (", c1, ",0) rectangle (", c2, ",0.5); \\node[anchor=center] (text) at (", t6, ",0.25){\\tiny F};")
print("  \draw[fill=red!30] (", c2, ",0) rectangle (", c3, ",0.5); \\node[anchor=center] (text) at (", t7, ",0.25){\\tiny M};")
print("  \draw[fill=orange!30] (", c3, ",0) rectangle (", c4, ",0.5);  \\node[anchor=center] (text) at (", t8, ",0.25){\\tiny E};")
print(" \end{scope}")
print("\end{tikzpicture}")
