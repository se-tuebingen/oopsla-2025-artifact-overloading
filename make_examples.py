#!/usr/bin/env python3

import json
from hashlib import md5

def make_examples(json_file):
    data = None
    with open(json_file, 'r', encoding='utf-8') as f:
        data = json.load(f)

    changes = []

    for obj in data:
        name = obj['name']
        path = obj['path']

        # Read file content
        try:
            with open(path, 'r', encoding='utf-8') as f:
                new_code = f.read()
        except FileNotFoundError:
            print(f"Error: File not found - {path}")
            continue

        # Check if changed
        new_hash = md5(new_code.encode()).hexdigest()
        old_hash = obj.get('_hash', '')

        if old_hash != new_hash:
            if '_hash' not in obj:
                changes.append(f"Added: {name}")
            else:
                changes.append(f"Modified: {name}")

            obj['code'] = new_code
            obj['_hash'] = new_hash

    # Save updated JSON
    with open(json_file, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=2)

    # Print changes
    if changes:
        print("Changes:")
        for change in changes:
            print(f"  {change}")
    else:
        print("No changes detected.")

if __name__ == "__main__":
    import sys

    if len(sys.argv) != 2:
        print("Usage: python make_examples.py <json_file>")
        sys.exit(1)

    make_examples(sys.argv[1])
