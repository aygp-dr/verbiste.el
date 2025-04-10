# Bug: Wrong type argument: char-or-string-p when using verb clusters

## Description
When using functions that access the verb clusters data (like `verbiste-display-similar-verbs`), an error occurs:
```
let: Wrong type argument: char-or-string-p, dominer
```

This happens because the JSON data structure for verb clusters can contain either symbols or strings as keys, and the code was not handling this mixed format correctly.

## Steps to Reproduce
1. Enable verbiste-mode
2. Try to use `verbiste-display-similar-verbs` with a verb like "dominer"
3. Error occurs when accessing the clusters data

## Fix
The fix involves:
1. Enhancing `verbiste--get-similar-verbs` to handle both string and symbol keys in the JSON data
2. Enhancing `verbiste--get-random-verbs` to ensure all verb names are returned as strings
3. Adding robust unit tests that verify the fix with mock data containing both symbol and string keys

## Root Cause
The root cause was that when JSON data is parsed with `json-read`, it can create either symbol or string keys depending on the `json-object-type` and `json-array-type` settings. The code was assuming a consistent format but the actual data contained a mix of formats.

## Additional Notes
This fix improves robustness by:
- Trying both string and symbol lookups
- Converting symbols to strings when needed
- Adding fallbacks for different JSON formats
- Providing default values for missing data

The added unit tests ensure the fix works with different data formats and edge cases.