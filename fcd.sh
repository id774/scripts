#!/bin/sh

# 1.1 -- 2008-03-26
#       fix a problem when items in group view is selected.
# 1.0.1 -- 2006.10.27
#       append error check for computer container and trash
# 1.0 -- 2004.04.22 

if targetDir=`osascript << EOS

on run
	tell application "Finder"
		set a_location to insertion location
	end tell
	
	try
		set a_class to class of a_location
		set insertion_location_path to a_location as Unicode text
	on error
		-- error occur when 
		-- * Finder window is in search mode i.e current view is group view
		-- * trash window is selected
		-- * network is selected
		-- a_location will be in valid value  ufolder ""v
		return POSIX path of (process_for_special_items() as alias)
	end try
	
	tell application "Finder"
		if a_class is not in {folder, disk} then
			if a_class is (class of computer container) then
				set a_location to missing value
			end if
		end if
	end tell
	
	set selected_location to location_for_selection()
	
	if selected_location is missing value then
		try
			set last_result to a_location as alias
		on error
			-- if computer container is selected, a_location will be missing value.
			set last_result to missing value
		end try
	else
		set last_result to selected_location as alias
	end if
	
	return POSIX path of last_result
end run

on get_container(an_item)
	-- use System Events to avoid Finder's problem that files in trash, folder property of items in trash return invalid value.
	tell application "System Events"
		return path of container of an_item
	end tell
end get_container

on location_for_selection()
	set a_location to missing value
	
	tell application "Finder"
		set selected_items to selection
		if selected_items is {} then
			return a_location
		end if
		
		set an_item to item 1 of selected_items
		if (an_item is not computer container) and ((an_item as Unicode text) ends with ":") then
			set a_location to an_item
		else if (class of an_item is alias file) then
			set an_original to original item of an_item
			if (an_original as Unicode text) ends with ":" then
				set a_location to an_original
			else
				--set a_location to folder of an_original
				--set a_location to my get_container(an_original as alias)
				set a_location to my get_container(an_item as alias)
			end if
		else
			set a_location to my get_container(an_item as alias)
		end if
	end tell
	
	return a_location
end location_for_selection

on process_for_special_items()
	-- process for ...
	-- * Finder window is in search mode i.e current view is group view
	-- * trash window is selected
	-- * network is selected -- will return missing value
	
	tell application "Finder"
		if not (exists Finder window 1) then
			return missing value
		end if
		
		set a_name to name of Finder window 1
		if a_name is displayed name of trash then
			set a_location to my location_for_selection()
			if a_location is missing value then
				set a_location to my trash_path()
			end if
			return a_location
			
		else if current view of Finder window 1 is group view then
			return my location_for_selection()
			
		else -- Network and Unknown
			return missing value
		end if
	end tell
end process_for_special_items

on trash_path()
	return path to trash
end trash_path

EOS`
then
	cd "$targetDir"
	echo $PWD
fi