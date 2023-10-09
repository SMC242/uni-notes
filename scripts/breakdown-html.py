from argparse import ArgumentParser
import fileinput
import enum

class Format(enum.Enum):
    # # Pros
    # - Pro1
    # # Cons
    # - Con1
    HEADING = enum.auto()
    # Pros:
    # - Pro1
    # Cons:
    # - Con1
    COLON = enum.auto()
    # Pro: Pro1
    # Con: Con1
    INLINE = enum.auto()


def categorise_format(lines: list[str]) -> Format:
    FORMAT_TELLS = {
        "# ": Format.HEADING,
        "pros:"
    }
    first_line = lines[0].lower()
    if "#" in first_line:
        return Format.HEADING
    elif "pros:" in first_line:
        return Format.COLON
    
    

def main():
    parser = ArgumentParser(prog="breakdown-html", usage="breakdown-html FILE", description="""
                    Convert a markdown list to the `breakdown` format. See docs/CSS/Breakdown.md
                            
                    The input markdown should be in one of the following formats:
                        1:
                            Pros:
                            - Pro1
                            - Pro2
                            - Pro3
                            Cons:
                            - Con1
                            - Con2
                            - Con3
                        2:
                            - Pro: Pro1
                            - Pro: Pro2
                            - Con: Con1
                            - Con: con2
                        3 (NOTE: can be any heading level so long as they are the same):
                            # Pros
                            - Pro1
                            # Cons
                            - Con1
                            """)
    
    parser.add_argument("file", metavar="FILE", type=str, help="The file path or stream containing the markdown list to convert to HTML")
    
    args = parser.parse_args()
    for line in fileinput.input([args.file] if not args.file else ["-"], encoding="utf-8"):
        print(line)
    

if __name__ == "__main__":
    main()