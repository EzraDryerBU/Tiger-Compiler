import os
import re
import subprocess
import sys

exe = "treecheck"

def prRed(skk): print("\033[91m{}\033[00m" .format(skk))

def prGreen(skk): print("\033[92m{}\033[00m" .format(skk))

def findEXE():
    for filename in os.listdir("."):
        if filename.startswith(exe):
            print(filename)
            return filename

def runFiles(folder, expected):
    total = 0;
    corr = 0;

    exe = findEXE()

    for filename in filter(lambda f: f.endswith(".tig"), os.listdir(folder)):
        total += 1;
        print(filename)

        incorrect = False

        # run the program
        expected_in_fn = "in/" + filename.replace(".tig", ".txt");
        input_str = None
        if os.path.isfile(expected_in_fn):
            input_str = open(expected_in_fn, "r")

        res = subprocess.run(["sml", "@SMLload=./" + exe, folder + filename], timeout = 2, capture_output = True, stdin = input_str)
        out = res.stdout.decode('utf-8').strip()

        pos1 = 0
        pos2 = 0
        try:
            pos1 = out.index("INTERPRETER")
            pos2 = out.index("\nEND INTERPRETER")
        except:
            pass

        final_out = ""
        for p in range(pos1 + 12, pos2):
            final_out = final_out + out[p]

        expected_out_fn = "out/" + filename.replace(".tig", ".txt");

        if len(sys.argv) >= 2 and sys.argv[1] == "--out":
            with open(expected_out_fn, "w") as out_file:
                out_file.write(final_out);
        else:
            with open(expected_out_fn, "r") as out_file:
                correct = out_file.read()
                if correct.strip() != final_out.strip():
                    print(correct)
                    print(final_out)
                    incorrect = True


        if incorrect:
            prRed("\tIncorrect");
        else:
            corr = corr + 1;
            prGreen("\tCorrect");

    return (corr, total)

subprocess.run(["ml-build", "sources.cm", "Main.compile", exe])
(c, t) = runFiles("./testcases/", 2)

print(str(c) + "/" + str(t))
