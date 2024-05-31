import os
import pexpect
import re
import sys


def prRed(skk): print("\033[91m{}\033[00m" .format(skk))

def prGreen(skk): print("\033[92m{}\033[00m" .format(skk))

child = pexpect.spawn("sml")

# wait for sml to be ready
child.expect("\n\-")

# compile
child.send("CM.make \"sources.cm\";\n");
child.expect("\n\-")

def runFiles(folder, expect):
    total = 0;
    corr = 0;

    for filename in os.listdir(folder):
        total += 1;

        print(filename)
        cmd = "Parse.parse(\"" + folder + filename + "\");"
        child.send(cmd + "\n")
        child.expect_exact(cmd + "\r\n")
        try:
            res = child.expect([ "(Error)", "(error)", "(.*)\nval"], timeout=2)
            if res == expect:
                res_m = child.match.group(1).decode().replace("\r", "")
                res_f = ""
                for line in res_m.splitlines():
                    no_num_line = re.sub("\s*\d+$", "", line)
                    res_f = res_f + "\n" + no_num_line
                # print(res_m)

                out_filename = "out/" + filename.replace(".tig", ".txt");
                if len(sys.argv) >= 2 and sys.argv[1] == "--out":
                    with open(out_filename, "w") as out_file:
                        out_file.write(res_f);
                else:
                    with open(out_filename, "r") as out_file:
                        correct = out_file.read();
                        if correct.strip() == res_f.strip():
                            corr += 1;
                            prGreen("\tCorrect");
                        else:
                            prRed("\tIncorrect");
            else:
                prRed("\tIncorrect");
        except pexpect.TIMEOUT:
                        prRed("\tTimeout");

    return (corr, total)


(c1, t1) = runFiles("./testcases/lex/", 2)
(c2, t2) = runFiles("./testcases/nolex/", 0)

print(str(c1 + c2) + "/" + str(t1 + t2))

child.close()

