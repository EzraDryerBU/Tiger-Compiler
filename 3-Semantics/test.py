import os
import re
import subprocess

exe = "semcheck"

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

        all_errors = ["TYPE", "SCOPE", "LOOP", "DUPLICATE", "READONLY", "MISPLACED"]
        expected_err = []

        # get expected errors
        with open(folder + filename) as f:
            err_line = f.readline()
        re_res = re.match("^/\*\|(.*)\|\*/\n$", err_line)
        if re_res:
            expected_err = re_res.group(1).split()

        # run the semantic checker
        res = subprocess.run(["sml", "@SMLload=./" + exe, folder + filename], timeout = 2, capture_output = True)
        out = res.stdout.decode('utf-8')

        for err in all_errors:
            if out.find(err) != -1 and err not in expected_err:
                print("1 " + err)
                incorrect = True
            if out.find(err) == -1 and err in expected_err:
                print("2 " + err)
                incorrect = True
        
        if out.find("uncaught exception Match") != -1:
            incorrect = True
        if incorrect:
            prRed("\tIncorrect");
        else:
            corr = corr + 1;
            prGreen("\tCorrect");

    return (corr, total)

subprocess.run(["ml-build", "sources.cm", "Main.compile", exe])
(c, t) = runFiles("testcases/", 2)

print(str(c) + "/" + str(t))
