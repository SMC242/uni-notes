import yaml
from argparse import ArgumentParser
from os import PathLike
from typing import  TypeAlias
from collections.abc import Iterable

Path: TypeAlias = str | bytes | PathLike

def flatten_dict(d: dict, parent: dict | None=None) -> dict:
    """Flatten nested dictionaries into one level"""
    parent = parent if parent is not None else {}
    for key, value in d.items():
        if isinstance(value, dict):
            flatten_dict(value, parent=parent)
        else:  parent[key] = value
    return parent

# def read_front_matter(f: Iterable[str]) -> str | None:
#     lines = iter(f)
#     try:
#         first_line = next(lines)
#         if "---" not in first_line:  return None
        
#         buffer = ""
#         while "---" not in (line := next(lines)):
#             buffer += f"{line}\n"
#         return buffer
#     except StopIteration:
#         return None
    
def read_front_matter(lines: list[str]) -> list[str] | None:
    if not lines or ("---" not in lines[0]):  return None
    front_matter = []
    i = 1
    try:
        while "---" not in lines[i]:
            front_matter.append(lines[i])
            i += 1
    except IndexError:  return None
    return front_matter

def migrate(fp: Path) -> None:
    with open(fp, "r+") as f:
        file_contents = f.readlines()
        front_matter = read_front_matter(file_contents)
        if front_matter is None:  return print("No frontmatter detected")
        
        data = yaml.load("\n".join(front_matter), yaml.Loader)
        new_data = flatten_dict(data)
        new_front_matter = f"---\n{yaml.dump(new_data)}\n---"
        
        file_body = file_contents[len(front_matter) + 2:]  # Remove frontmatter
        file_body.insert(0, new_front_matter)
        output = "\n".join(file_body)
        
        f.seek(0)
        f.write(output)
    

def main():
    parser = ArgumentParser("migrate-properties")
    parser.add_argument("FILE", type=str, help="The Obsidian-flavoured markdown file to migrate")
    
    args = parser.parse_args()
    
    migrate(args.FILE)
    
    
if __name__ == "__main__":
    main()