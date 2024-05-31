import os
import pexpect
import sys 

child = None

def prRed(skk): print("\033[91m{}\033[00m" .format(skk))

def prGreen(skk): print("\033[92m{}\033[00m" .format(skk))

def initSML():
    global child
    if child is not None:
        child.close()

    child = pexpect.spawn("sml")

    # wait for sml to be ready
    child.expect("\n\-")

    # compile
    child.send("CM.make \"sources.cm\";\n");
    child.expect("\n\-")
    return child

def runFiles(folder, expected):
    total = 0;
    corr = 0;

    for filename in filter(lambda f: f.endswith(".tig"), os.listdir(folder)):
        total += 1;

        print(filename)
        cmd = "Parse.parse(\"" + folder + filename + "\");"
        child.send(cmd + "\n")
        child.expect_exact(cmd + "\r\n")
        try:
            res = child.expect([ "(Error)", "(error)", "(.*)\nval"], timeout=2)

            if res in expected:
                res_m = child.match.group(1).decode().replace("\r", "")

                out_filename = "out_part2/" + filename.replace(".tig", ".txt");
                if len(sys.argv) >= 2 and sys.argv[1] == "--out":
                    with open(out_filename, "w") as out_file:
                        out_file.write(res_m);
                else:
                    with open(out_filename, "r") as out_file:
                        correct = out_file.read();
                        if correct.strip() == res_m.strip():
                            corr += 1;
                            prGreen("\tCorrect");
                        else:
                            prRed("\tIncorrect");
            else:
                prRed("\tIncorrect");

            if res == 0 or res == 1:
                initSML()

        except pexpect.TIMEOUT:
            initSML()
            prRed("\tTimeout");
    return (corr, total)

initSML()
(c1, t1) = runFiles("./testcases/parse/", [2]);
(c2, t2) = runFiles("./testcases/noparse/", [0, 1]);

print(str(c1 + c2) + "/" + str(t1 + t2))

child.close()

