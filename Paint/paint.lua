-- In conjunction with the corresponding instrumented paint code,
-- this will help visualize the stack of work items used by paint,
-- animating the pushes and pulls. 
--
-- Load into MAME via -script
--
-- See comments in PAINTDRG.ASM and PAINT.BAS for more info.
--
-- You may find this useful as an example of how to do some things
-- with lua MAME scripting. But this code is likely NOT a good example
-- of how to write well-styled lua, since I have very little
-- experience writing lua. 

print("lua says... " .. emu.app_name() .. " " .. emu.app_version())
cpu = manager.machine.devices[':maincpu']
mem = cpu.spaces["program"]
screen = manager.machine.screens[':screen']

AnimationStates =
{
    -- No animation in progress.
    NONE = 0,

    -- Animating tip of stack rising up.  Completion of the up
    -- animation sends us to next state
    UP = 1,

    -- Animating tip of stack moving right across top margin until it
    -- lands in the center.  Completion of the
    -- right animation sends us back to NONE
    RIGHT = 2
}

-- Constants: behavior / appearance
useWithDFS = false                  -- Adapt behavior to work better with PAINT or DFS (latter inhibits animation)
listEntryTextLineHeight = 6
listEntryBoxHeight = 1 * (listEntryTextLineHeight + 2)
if useWithDFS then
    listEntryBoxWidth = 30
else
    listEntryBoxWidth = 50
end
listEntryByteCount = 6
xLeft = 5
yUp = 20
xRight = xLeft + listEntryBoxWidth
yDn = 242
curParamXLeft = xRight+30
curParamYUp = 10
maxNumItemsXLeft = curParamXLeft + 2 * listEntryBoxWidth
maxNumItemsYUp = curParamYUp
maxNumItemsBoxWidth = 75
maxNumItemsBoxHeight = listEntryBoxHeight
numEntriesPerColumn = (yDn - yUp) // listEntryBoxHeight;
WHITE               = 0xffffffff
TRANSPARENT_BLACK   = 0x90000000

-- Constants: addresses for interacting with instrumented paint
LIST_START_TRIGGER =  0x7FF0
LIST_PUSH_TRIGGER =   0x7FF1
LIST_PULL_TRIGGER =   0x7FF2
LIST_START_ADDR =     0x7FF3
LIST_END_ADDR =       0x7FF5
CUR_PARAMS_ADDR =     0x7FF7

-- Globals
listStart = 0
listEnd = 0
currentParams = 0
curParamStr = ''
animationListEntries = {}
maxNumItems = 0

-- Called once per frame to do all drawing
function draw_overlay()
    if listStart == 0 or listEnd == 0 or currentParams == 0 then
        -- paint not in progress, so don't show anything
        return
    end

    -- Print stack list
    numItems = get_num_list_entries()
    for item = 0, numItems - 1, 1 do
        -- determine where to display stack entry
        loc = calculate_list_entry_location(item)

        -- bounding box
        screen:draw_box(
            loc.x,
            loc.y,
            loc.x + listEntryBoxWidth,
            loc.y + listEntryBoxHeight,
            WHITE,
            TRANSPARENT_BLACK)

        itemContentsAddrTip = listStart - ((item + 1) * listEntryByteCount)
        listEntryStr = read_list_entry(itemContentsAddrTip)

        -- print entry
        screen:draw_text(loc.x + 1, loc.y, listEntryStr)
    end

	-- update animation state
    advance_animation_data()

	-- Print current params
    if curParamStr ~= '' then
        screen:draw_box(curParamXLeft, curParamYUp, curParamXLeft + listEntryBoxWidth, curParamYUp + listEntryBoxHeight)
        screen:draw_text(curParamXLeft+1, curParamYUp, curParamStr)
    end

    -- Print pulling params wherever animation data says they should go
	-- (potentially partially on top of current params)
    display_animation_data()

    -- Print peak num items
    if numItems > maxNumItems then
        maxNumItems = numItems
    end
    screen:draw_box(maxNumItemsXLeft, maxNumItemsYUp, maxNumItemsXLeft + maxNumItemsBoxWidth, maxNumItemsYUp + maxNumItemsBoxHeight)
    screen:draw_text(maxNumItemsXLeft+1, maxNumItemsYUp, "Max stack items: " .. maxNumItems)
end

-- Given a work item's ordinal, determines where it should be drawn
function calculate_list_entry_location(item)
    ret = {}
    colNum = item // numEntriesPerColumn
    rowNum = item % numEntriesPerColumn
    ret.x = xLeft + (colNum * listEntryBoxWidth)
    ret.y = yDn - ((rowNum + 1) * listEntryBoxHeight)
    return ret
end

-- Update animation data by one frame.  Does no drawing.
function advance_animation_data()
    -- Iterate in reverse order so table.remove calls don't mess up 
    -- indices we're about to use
    foundCurParamStr = false
    for i = #animationListEntries,1,-1 do
        -- update this entry
        advance_single_animation_data_entry(animationListEntries[i])
        -- deal with entries no longer animating
        if animationListEntries[i].animationState == AnimationStates.NONE then
            if not foundCurParamStr then
                -- The current params will be the most recent completed
                -- animated list entries.  (There can be multiple completed
                -- entries at a time when things move quickly, so be careful
                -- to only use the most recent one.)
                curParamStr = animationListEntries[i].animationListEntryStr
                foundCurParamStr = true
            end
            -- Remove all completed entries
            table.remove(animationListEntries, i)
        end
    end
end

-- Update animation data for a single work item by one frame.
-- Does no drawing.
function advance_single_animation_data_entry(entry)
    if (entry.animationState == AnimationStates.UP) then
        -- move up
        entry.animationYUp = entry.animationYUp - 10
        -- check for transition to RIGHT
        if (entry.animationYUp <= curParamYUp) then
            entry.animationYUp = curParamYUp
            entry.animationState = AnimationStates.RIGHT
        end
    elseif (entry.animationState == AnimationStates.RIGHT) then
        -- move right
        entry.animationXLeft = entry.animationXLeft + 4
        -- check for transition to NONE
        if (entry.animationXLeft >= curParamXLeft) then
            entry.animationXLeft = curParamXLeft
            entry.animationState = AnimationStates.NONE
        end
    end
end

-- Draws all animated work items for this frame
function display_animation_data()
    for i = 1,#animationListEntries do
        entry = animationListEntries[i]
        if (entry.animationListEntryStr ~= '') then
            screen:draw_box(
                entry.animationXLeft,
                entry.animationYUp,
                entry.animationXLeft + listEntryBoxWidth,
                entry.animationYUp + listEntryBoxHeight)
            screen:draw_text(entry.animationXLeft+1, entry.animationYUp, entry.animationListEntryStr)
        end
    end
end

function get_num_list_entries()
    return (listStart - listEnd) // listEntryByteCount
end

-- read list entry contents (ascending address order):
-- direction (byte)
-- Y (byte)
-- X (word)
-- point count (word)
function read_list_entry(itemContentsAddrTip)
    dirVal = mem:read_u8(itemContentsAddrTip)
    if dirVal == 0 then
        return 'base'
    end
    listEntry = {}
    listEntry.dirStr = '?'
    yAdjustment = 0
    if dirVal == 1 then
        listEntry.dirStr = 'S'
        yAdjustment = 1
    elseif dirVal == 0xFF then
        listEntry.dirStr = 'N'
        yAdjustment = -1
    end

    if listEntry.dirStr == '?' then
        return '?'
    end
    
    -- Y is always incremented or decremented after pulled & before use, based on the
    -- N/S direction, so do the same here for clarity on what this list entry really represents
    listEntry.yVal = mem:read_u8(itemContentsAddrTip + 1) + yAdjustment
    
    -- X is always incremented after pulled & before use, so do the same here
    -- for clarity on what this list entry really represents
    listEntry.xVal = mem:read_i16(itemContentsAddrTip + 2) + 1
    listEntry.pointCountVal = mem:read_u16(itemContentsAddrTip + 4)

    if useWithDFS then
        -- DFS stack only shows pixel coordinates
        return string.format('(%d,%d)', listEntry.xVal, listEntry.yVal)
    end

    -- Regular PAINT stack shows all the goodies
    return string.format('(%d,%d) %d %s', listEntry.xVal, listEntry.yVal, listEntry.pointCountVal, listEntry.dirStr)
end

-- Triggered on start of paint to initialize variables
function captureListStart(offset, val, mask)
    listStart = mem:read_u16(LIST_START_ADDR)
    listEnd = mem:read_u16(LIST_END_ADDR)
	currentParams = mem:read_u16(CUR_PARAMS_ADDR)
	curParamStr = ''
    maxNumItems = 0
	animationListEntries = {}
end

-- Triggered on each push to update variables
function captureListPush(offset, val, mask)
    listEnd = mem:read_u16(LIST_END_ADDR)
end

-- Triggered on each pull to update variables
function captureListPull(offset, val, mask)
    -- Pull changes the list end, so update
    listEnd = mem:read_u16(LIST_END_ADDR)

    -- Read current params
    currentParams = mem:read_u16(CUR_PARAMS_ADDR)

    -- Init animation data for regular paint (DFS stack too busy)
    if useWithDFS then
        return
    end
    newAnimationListEntry = {}
    newAnimationListEntry.animationState = AnimationStates.UP
    newAnimationListEntry.animationListEntryStr = read_list_entry(currentParams)
    print("Pull says: " .. newAnimationListEntry.animationListEntryStr)
	if newAnimationListEntry.animationListEntryStr == '?' then
		-- Ignore bogus entries
		return
	end
    numListEntries = get_num_list_entries()
    newAnimationListEntry.animationXLeft = xLeft
    newAnimationListEntry.animationYUp = yDn - ((numListEntries + 2) * listEntryBoxHeight)

    -- add to end of list
    table.insert(animationListEntries, newAnimationListEntry)

end

-- Global code to connect above functions to events
emu.register_frame_done(draw_overlay, 'frame')
handler1 = mem:install_write_tap(LIST_START_TRIGGER, LIST_START_TRIGGER, "captureListStart", captureListStart)
handler2 = mem:install_write_tap(LIST_PUSH_TRIGGER, LIST_PUSH_TRIGGER, "captureListPush", captureListPush)
handler3 = mem:install_write_tap(LIST_PULL_TRIGGER, LIST_PULL_TRIGGER, "captureListPull", captureListPull)

