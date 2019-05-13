import os

def process(s):
    if s.startswith("import "):
        return [s[len("import"):].strip()]
    if s.startswith("from") and "import" in s:
        pos_import = s.find("import")
        end = s.find("#") if "#" in s else len(s)
        return ["{}.{}".format(s[5:pos_import].strip(), x[:x.find("as") if "as" in x else len(x)].strip()) for x in s[pos_import+len("import"):end].split(",")]
    return []

assert process("import perf\n") == ["perf"]
assert process("from tornado.web import RequestHandler, Application\n") == ["tornado.web.RequestHandler", "tornado.web.Application"]
assert process("from mako.lookup import TemplateLookup   # noqa") == ["mako.lookup.TemplateLookup"]
process("from six import u as u_lit, text_type") == ["six.u_lit", "six.text_type"]

def find_deps(fi):
    r = []
    lc = 0
    with open(fi, 'r') as f:
        for line in f:
            lc += 1
            r += process(line)
    return "| {} | {} | {} |".format(fi, lc, ", ".join(sorted(r)))

if __name__ == "__main__":
    print("#+OPTIONS: ^:nil")
    print()
    print("|-+-+-|")
    print("| filename | loc | dependencies |")
    print("|-+-+-|")
    for f in sorted(os.listdir()):
        if f.startswith("bm_") and f.endswith(".py"):
            print(find_deps(f))
    print("|-+-+-|")
