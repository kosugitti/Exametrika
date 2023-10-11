import os
import re
import openai
from difflib import SequenceMatcher

def list_files(directory, extensions):
    """List all files with specified extensions in the given directory and its subdirectories."""
    files = []
    for root, _, file_list in os.walk(directory):
        for file in file_list:
            if file.endswith(extensions):
                files.append(os.path.join(root, file))
    return files

def extract_variables_from_file(filename):
    """Extract R script object names from the given file."""
    with open(filename, 'r') as file:
        lines = file.readlines()

    pattern = re.compile(r'\b[a-zA-Z_][a-zA-Z0-9_]*\b')
    return [(filename, i+1, match) for i, line in enumerate(lines) for match in pattern.findall(line)]

def check_spelling(variable_name):
    """Check the spelling of the given variable name using chat-GPT API."""
    messages = [
        {"role": "system", "content": "You are a senior programmer."},
        {"role": "user", "content": f"Check the spelling of this variable name: {variable_name}."}
    ]
    res = openai.ChatCompletion.create(
        model='gpt-4',
        messages=messages
    )
    # ここでスペルミスの判断を行います。具体的な判断基準は、APIの返答に基づいて調整する必要があります。
    if "misspelled" in res.choices[0].message['content'].lower():
        return True
    return False

def main():
    target_directory = './path/to/directory'
    target_files = list_files(target_directory, ('.R', '.Rmd'))

    all_variables = [var for filename in target_files for var in extract_variables_from_file(filename)]

    misspelled_vars = [var for _, _, var in all_variables if check_spelling(var)]
    print("Misspelled Variables:", *misspelled_vars, sep='\n')

if __name__ == "__main__":
    main()
