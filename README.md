# Word Frequency
Count the frequency of words in a file or directory.

# Usage
word-frequency - counting manually is overrated

Usage: word-frequency-exe (-p|--path TARGET) [-l|--length INT] [-c|--count INT]
                          [--top]
  Count the frequency of words in a file or directory.

Available options:
  -p,--path TARGET         The file or directory to count words for.
  -l,--length INT          The minimum length of words to include. (default: 19)
  -c,--count INT           The minimum number of occurences a word must have to
                           be included.
  --top                    Display only the top 10 results.
  -h,--help                Show this help text

# Build Instructions
1. Clone this repository with `git clone`
2. Run `stack build`
3. Follow usage above or use `stack exec word-frequency-exe -- --path TARGET`

# Why
The very first script I ever wrote was a Python script to parse source code for frequently used variables. Word Frequency is a Haskell homage to this original script.
