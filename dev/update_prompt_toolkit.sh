#!/bin/bash

# File and line number to update
FILE_PATH="/opt/python/3.12/lib/python3.12/site-packages/prompt_toolkit/application/application.py"
LINE_NUM=988
OLD_CODE="loop = asyncio.get_event_loop()"
NEW_CODE="loop = asyncio.new_event_loop()"

# Check if the file exists
if [ -f "$FILE_PATH" ]; then
    # Check if the specific line contains the old code
    if sed -n "${LINE_NUM}p" "$FILE_PATH" | grep -q "$OLD_CODE"; then
        # Update the line with new code
        sed -i "${LINE_NUM}s/.*/$NEW_CODE/" "$FILE_PATH"
        echo "File updated successfully: $FILE_PATH"
    else
        echo "No update needed. Line $LINE_NUM does not contain the expected code."
    fi
else
    echo "Error: File does not exist - $FILE_PATH"
    exit 1
fi
