-- Display overlay to view in real-time the processes running under
-- NitrOS-9 Level II and their memory maps.
--
-- Load this script into MAME using the -script command-line argument.
-- Emulate a CoCo 3, then load NitrOS-9 Level II.
-- Once the loading has begun, it's safe to activate this script.  Hit
-- the backslash key (\) inside the emulated machine to toggle
-- activation of this script on and off.
--
-- More information can be found at https://www.youtube.com/@CocoTownRetro
-- or https://cocotownretro.wordpress.com

-- Just a little something to let us know you loaded
print("lua says... " .. emu.app_name() .. " " .. emu.app_version())

-- Constants that affect appearance
NUM_COLS = 6
NUM_ROWS = 3
TITLE_X_OFFSET = 10
TITLE_Y_OFFSET = 0
LOGICAL_LABEL_X_OFFSET = 10
LOGICAL_LABEL_Y_OFFSET = 8
LOGICAL_ADDR_X_OFFSET = 15
LOGICAL_ADDR_Y_OFFSET = 16
MAP_GRID_X_OFFSET = 50
MAP_GRID_Y_OFFSET = 16
MAP_WIDTH = 42
MAP_HEIGHT = 60
PHYSICAL_LABEL_X_OFFSET = 48
PHYSICAL_LABEL_Y_OFFSET = 8
PHYSICAL_ADDR_X_OFFSET = 55
PHYSICAL_ADDR_Y_OFFSET = 16
FULL_LEFT_OFFSET = 10
FULL_UP_OFFSET = 0
FULL_RIGHT_OFFSET = 62
FULL_DOWN_OFFSET = 21
SPACER_X = 5
SPACER_Y = 1
WHITE                           = 0xffffffff
GRAY                            = 0xffB0B0B0
TRANSBLACK                      = 0xB0000000
MAP_COLOR                       = 0xff00ffff
TRANS                           = 0x00000000
MODULE_ADDR_COLOR               = 0xff00ff00
CURRENT_PROC_HIGHLIGHT_COLOR    = 0xffff0000

-- WARNING!! Hard-coded offsets based on os9.d and coco.d from NitrOS-9 EOU 1.0.1 (April 7, 2024)
-- Dunno how frequently these change, but if they do, this script will break.
DATdFree = 0x333E
DATdBlCt = 8
DATdBlSz = (256/DATdBlCt)*256
DdPrcDBT = 0x48
PdPID = 0
PdPModul = 0x11
PdDATImg = 0x40
PdState = 0xC
DdSysDAT = 0x4C
DdProc = 0x50
Dead = 1
MdName = 0x4

-- Globals.  Not all are used, but keeping them around to remind me how to
-- get them from MAME should I need them in the future
cpu = manager.machine.devices[':maincpu']
mem = cpu.spaces["program"]
screen = manager.machine.screens[':screen']
input = manager.machine.input
gime = manager.machine.devices[':gime']
ram = manager.machine.devices[':ram']
physIdx = ram.items["0/m_pointer"]
physItem = emu.item(physIdx)
toggleOverlayPressedPrev = false
showOverlay = false
skipCount = 0
-- logFile = io.output("c:\\temp\\out.txt")

-- Helper to convert an array (memory map) to a string for printing
function table_to_string(tbl)
    ret = ''
    for tag,val in pairs(tbl) do
        ret = ret .. '[' .. tag .. '] = ' .. val .. ', '
    end
    return ret
end

-- Write a message to debugging log file
function log(msg)
    -- logFile:write(msg .. '\n')
end

-- Given logical address, memory map, and a debugging explanatory string,
-- read and return the byte at that address
function read_byte_from_map(logicalAddr, map, what)
    log("read_byte_from_map " .. what .. ": " .. logicalAddr .. ", " .. table_to_string(map))
    assert_word(logicalAddr)
    index = logicalAddr // 0x2000
    offset = logicalAddr % 0x2000
    if map[index] > 0xFF then
        return 0
    end
    physAddr = map[index] * 0x2000 + offset
    return physItem:read(physAddr)
end

-- Given logical address, memory map, and a debugging explanatory string,
-- read and return the 16-bit word at that address
function read_word_from_map(logicalAddr, map, what)
    log("read_word_from_map " .. what .. ": " .. logicalAddr .. ", " .. table_to_string(map))
    assert_word(logicalAddr)
    hi = read_byte_from_map(logicalAddr, map, what)
    lo = read_byte_from_map(logicalAddr + 1, map, what)
    return hi * 0x100 + lo
end

-- Helper to abort the script if an unexpected value results from
-- a memory read.  This can be triggered if the overlay is enabled
-- while not running NitrOS-9
function assert_word(word)
    if word > 0xFFFF then
        print(string.format('%X is not a word', word))
        showOverlay = false
        logFile:close()
        error("error")
    end
end

-- Called on every render frame to draw everything
function draw_overlay()
    -- Detect if backslash is pressed, to enable / disable the overlay.
    -- We really just want to call input:code_pressed_once, but it's not recommended for use in a
    -- "production script", as it will modify state in the emulated machine.  So
    -- we manually check if the state of the backslash key changed since last time
    toggleOverlayPressedNow = input:code_pressed(input:code_from_token("KEYCODE_BACKSLASH"))
    if not toggleOverlayPressedPrev and toggleOverlayPressedNow then
        showOverlay = not showOverlay
    end
    toggleOverlayPressedPrev = toggleOverlayPressedNow

    -- showOverlay is now reliable.  If false, we're done
    if not showOverlay then
        return
    end

    --
    -- Code below inspired by the assembly source code for F$GProcP, PMap utility
    --

    -- Grab system map from physical 0-page
    sysMap = {}
    sysMapAddr = physItem:read(DdSysDAT) * 0x100 + physItem:read(DdSysDAT + 1)
    for i = 0,7,1 do
        datWord = physItem:read(sysMapAddr + 2*i) * 0x100 + physItem:read(sysMapAddr + 2*i + 1)
        if datWord ~= DATdFree then
            datWord = datWord & 0x00FF
        end       
        sysMap[i] = datWord
    end

    -- Find current pid
    currentProcDescr = read_word_from_map(DdProc, sysMap, "current proc descr")
    currentPid = read_byte_from_map(currentProcDescr + PdPID, sysMap, "current pid")

    -- Iterate through all possible process IDs
    mapNum = 0
    procDescrHiAddrBase = read_word_from_map(DdPrcDBT, sysMap, "procDescrHiAddrBase")
    for pid = 1,255,1 do
        procDescrHiAddr = procDescrHiAddrBase + pid
        procDescrHi = read_byte_from_map(procDescrHiAddr, sysMap, "procDescrHi " .. pid)
        if procDescrHi == 0 then
            goto continue
        end
        procDescr = procDescrHi * 0x100

        -- Get PID from structure (seems to always match pid var, but pmap does it
        -- this way, so I will too)
        displayPid = read_byte_from_map(procDescr + PdPID, sysMap, "iterated pid")

        -- Read this process's memory map
        procMap = {}
        for i = 0,7,1 do
            datWord = read_word_from_map(procDescr + PdDATImg + (i*2), sysMap, "datWord " .. i)
            if datWord ~= DATdFree then
                datWord = datWord & 0x00FF
            end
            procMap[i] = datWord
        end
    
        -- Grab this process's name
        procName = ''
        moduleAddr = 1      -- Deliberately init to a number that won't match a 2K boundary so it won't be highlighted
        processState = read_byte_from_map(procDescr + PdState, sysMap, "processState")
        if processState & Dead ~= 0 then
            procName = 'Dead'
        else
            moduleAddr = read_word_from_map(procDescr + PdPModul, sysMap, "moduleAddr")
            if moduleAddr == 0 then
                procName = 'System'
                procMap = sysMap
                moduleAddr = 1      -- Reset; no module for system
            else
                -- Typical case.  Not dead, not system.  Read 2-byte name offset
                nameOffset = read_word_from_map(
                    moduleAddr + MdName, 
                    procMap, 
                    string.format('name offset from %X + %X', moduleAddr, MdName))
                for i = 0,30,1 do
                    nameChar = read_byte_from_map(
                        moduleAddr + nameOffset + i, 
                        procMap, 
                        string.format('nameChar from %X + %X + %X', moduleAddr, nameOffset, i))
                    isLastChar = false
                    if nameChar & 128 ~= 0 then
                        isLastChar = true
                        nameChar = nameChar & 127
                    end
                    procName = procName .. string.char(nameChar)
                    if isLastChar then
                        break
                    end
                end
            end
        end

        -- Information is finally gathered; time to display
        if procName ~= '' then
            draw_map(
                mapNum, 
                string.format("%d %s", displayPid, procName), 
                procMap, 
                moduleAddr, 
                displayPid == currentPid)
            mapNum = mapNum + 1
        end
        ::continue::
    end
end

-- Heler to draw memory map of single process
--  - mapNum: Ordinal for map to help us know where to draw it
--  - title: String of process ID & name to display on top
--  - map: Memory map to display
--  - moduleAddr: Logical address where module is loaded, so we know to highlight it
--  - isCurrent: Bool to tell us if this process is the currently executing one,
--               so we know to highlight it with a surrounding box
function draw_map(mapNum, title, map, moduleAddr, isCurrent)
    row = mapNum // NUM_COLS
    col = mapNum % NUM_COLS
    xLeft = col*(FULL_RIGHT_OFFSET + MAP_WIDTH + SPACER_X)
    yUp = row*(FULL_DOWN_OFFSET + MAP_HEIGHT + SPACER_Y)
    xRight = xLeft + MAP_WIDTH
    yDn = yUp + MAP_HEIGHT

    -- background
    lineColor = TRANSBLACK
    if isCurrent then
        lineColor = CURRENT_PROC_HIGHLIGHT_COLOR
    end
    screen:draw_box(
        xLeft + FULL_LEFT_OFFSET,
        yUp + FULL_UP_OFFSET, 
        xRight + FULL_RIGHT_OFFSET, 
        yDn + FULL_DOWN_OFFSET, 
        lineColor, 
        TRANSBLACK)

    -- labels
    screen:draw_text(xLeft + TITLE_X_OFFSET, yUp + TITLE_Y_OFFSET, title, WHITE)
    screen:draw_text(xLeft + PHYSICAL_LABEL_X_OFFSET, yUp + PHYSICAL_LABEL_Y_OFFSET, "physical", GRAY)
    screen:draw_text(xLeft + LOGICAL_LABEL_X_OFFSET, yUp + LOGICAL_LABEL_Y_OFFSET, "logical", GRAY)

    -- bounding box for map
    screen:draw_box(
        xLeft + MAP_GRID_X_OFFSET,
        yUp + MAP_GRID_Y_OFFSET,
        xRight + MAP_GRID_X_OFFSET,
        yDn + MAP_GRID_Y_OFFSET,
        MAP_COLOR,
        TRANS)

    -- Go down row-by-row through the map
    memLabel = 0
    y = yUp
    for idx = 0, 7, 1 do
        -- Draw logical addresses outside grid
        logColor = GRAY
        physColor = WHITE
        if memLabel == moduleAddr then
            logColor = MODULE_ADDR_COLOR
            physColor = MODULE_ADDR_COLOR
        end
        screen:draw_text(
            xLeft + LOGICAL_ADDR_X_OFFSET, 
            y + LOGICAL_ADDR_Y_OFFSET, 
            string.format('%04X', memLabel),
            logColor)

        -- Draw physical address inside map grid
        physAddr = '..'
        if map[idx] ~= DATdFree then
            physAddr = string.format('%05X', map[idx] * 0x2000)
        end
        screen:draw_text(
            xLeft + PHYSICAL_ADDR_X_OFFSET, 
            y + PHYSICAL_ADDR_Y_OFFSET, 
            physAddr,
            physColor)
        
        -- Draw row separator
        screen:draw_line(
            xLeft + MAP_GRID_X_OFFSET, 
            y + MAP_GRID_Y_OFFSET, 
            xRight + MAP_GRID_X_OFFSET, 
            y + MAP_GRID_Y_OFFSET, 
            MAP_COLOR)

        -- Update for next row
        memLabel = memLabel + 0x2000
        y = y + (yDn - yUp)/8
    end
end

-- Tell MAME to call draw_overlay on every frame render
emu.register_frame_done(draw_overlay, 'frame')
