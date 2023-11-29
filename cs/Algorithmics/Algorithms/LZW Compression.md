---
"tags:": Compression
---
# Overview
LZW (Lempel-Ziv-Welch) compression is a lossless data compression algorithm that excels at compressing structured data, especially text. It was originally developed by Abraham Lempel and Jacob Ziv and later refined by Terry Welch. LZW works by replacing repeated sequences of data with shorter codes, resulting in efficient data storage and transmission.

# Properties
- **Time complexity**:
	- Best case: $O(n)$, where $n$ is the length of the input data.
	- Worst case: $O(n)$, where $n$ is the length of the input data.
- **Space complexity**: $O(m)$, where $m$ is the size of the dictionary used by the algorithm.
- **Strategy type**: LZW employs a dictionary-based compression strategy, dynamically building and maintaining a dictionary of encountered sequences.
- **Stable**: Yes, LZW is a stable algorithm, as it allows for the perfect reconstruction of the original data from the compressed data.
- **Recursive**: No, LZW compression does not involve recursion.

# Use case
LZW compression is particularly useful in the following scenarios:
- **Text Compression**: LZW is highly effective for compressing textual data, making it suitable for file compression formats like GIF and TIFF.
- **Network Data Compression**: It can significantly reduce the size of data transmitted over networks, improving data transfer speeds.
- **Storage Space Optimization**: LZW helps save storage space by efficiently compressing files, documents, and data structures.

# Vanilla implementation
The LZW compression process consists of the following sub-procedures:

1. **Initialization**: Initialize a dictionary with entries for all possible symbols (e.g., characters in the input data).

2. **Encoding**:
   - Start with an empty buffer.
   - Read the input data symbol by symbol.
   - Append the current symbol to the buffer.
   - Check if the buffer content exists in the dictionary:
     - If it exists, continue reading and appending symbols to the buffer until the longest sequence not in the dictionary is found.
     - Output the code for the sequence found in the dictionary.
     - Add the new sequence (buffer + next symbol) to the dictionary.
     - Clear the buffer and start again.
   - Repeat this process until the entire input data is processed.

3. **Output the codes**: The output of the encoding process is a sequence of codes representing the compressed data.

# Variants
- Variations of the LZW algorithm, such as LZW-12, use different code sizes for encoding, which may be optimized for specific applications or data types.
- LZW compression is used as a base algorithm in other compression formats, including GIF (Graphics Interchange Format) and TIFF (Tagged Image File Format), each with its own adaptations and variations.