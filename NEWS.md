# Version 0.1.3

Add few enhencements:
- Add a `All` and `Clear` button to select inputs
- Add spinner to indicate a long running process
- Suppress the messages from the main R console
- Suppress some useless message from the `coalesce_join` function
- Gracefully abort if no folder is found in the input directory
- Ensure that all the inputs are filled before running the script
- Clear the fake console before a new run
- Imporve some code in the `combine_data.R`
- Some aesthetics improvements

# Version 0.1.2

Bug fixes:
- Improve `CO2ATM` files parsing to cover more problematic situations
- Correct the `appendData` function
- Change `parseDate` function to round systematicaly the time to 10 mins
- Ignore empty files and empty data frames
- Mkae the `coalesce_join` function df agnostic
- Increase the `maxgap` size for the data interpolation to 5

Other changes:
- Change some styling
- Increase addin window size

# Version 0.1.1

Minor bug fix in the `combine_data.R` logic.

- `parseNDEPTH` is now keeping only the first occurence of columns starting with either `wtemp` and `wtrhgt`.


# Version 0.1.0

Initial Release
