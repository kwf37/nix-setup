-- This is a lua script for use in Conky.
require 'cairo'

-- Variables for tracking brightness changes
local brightness = conky_parse("${exec light -G}")
local prev_brightness = brightness

-- Variables for tracking volume changes
local volume = 0.0
local prev_volume = volume
local muted = false
local prev_muted = muted

function update_volume ()
    prev_volume = volume
    prev_muted = muted

    -- Check current volume
    raw_volume = conky_parse("${exec wpctl get-volume @DEFAULT_AUDIO_SINK@}")
    words = {}
    -- wpctl prints the volume as "Volume: 0.50 [Muted["
    for word in raw_volume:gmatch("%S+") do table.insert(words, word) end
    -- The second word is the volume
    volume = tonumber(words[2])
    -- The third value, if present, means the volume is muted
    muted = words[3] ~= nil
end

-- Variables for controlling fadeout
local stale_counter = 0
local opacity = 1.0

-- Variable for display
local display = "brightness"


function update_state ()
    -- Update Brightness
    prev_brightness = brightness
    brightness = conky_parse("${exec light -G}")

    -- Update Volume
    update_volume ()

    -- Check for changes
    if brightness == prev_brightness and volume == prev_volume and muted == prev_muted then
        if opacity > 0 then
            stale_counter = stale_counter + 1
        end
    else
        stale_counter = 0
        opacity = 1.0
    end

    -- Fade out the display using opacity
    if stale_counter > 50 and opacity > 0 then opacity = opacity - 0.05
    end

    -- Determine which bar to show
    if brightness ~= prev_brightness then
        display = "brightness"
    elseif volume ~= prev_volume or muted ~= prev_muted then
        display = "volume"
    end
end

function draw (cr)
    -- Draw Background
    bg_width = conky_window.width
    bg_height = conky_window.height
    bg_x = 0
    bg_y = 0
    bg_rgba = {0, 0, 0, 0.8 * opacity}

    cairo_set_source_rgba (cr, bg_rgba[1], bg_rgba[2], bg_rgba[3], bg_rgba[4])
    cairo_rectangle(cr, bg_x, bg_y, bg_width, bg_height)
    cairo_fill(cr)

    -- Draw Brightness Bar
    bar_percent = 1.0
    if display == "brightness" then
        bar_percent = brightness
    elseif display == "volume" then
        if muted then
            bar_percent = 0
        else
            bar_percent = volume * 100
        end
    end
    bar_max_width = 300
    bar_width = bar_max_width * tonumber(bar_percent) / 100
    bar_height = 15 
    bar_x = (conky_window.width - bar_max_width) / 2
    bar_y = conky_window.height - (bar_height + 20)
    bar_rgba = {0.8, 0.8, 0.8, opacity}

    cairo_set_source_rgba (cr, bar_rgba[1], bar_rgba[2], bar_rgba[3], bar_rgba[4])
    cairo_rectangle(cr, bar_x, bar_y, bar_width, bar_height)
    cairo_fill(cr)

    -- Draw Label
    cairo_select_font_face (cr, "DejaVu Sans Mono", CAIRO_FONT_SLANT_NORMAL,
                               CAIRO_FONT_WEIGHT_BOLD);
    cairo_set_font_size (cr, 20);
    local extents = cairo_text_extents_t:create()
    local utf8 = ""
    if display == "brightness" then
        utf8 = "Brightness"
    elseif display == "volume" then
        if muted then
            utf8 = "Volume [MUTED]"
        else
            utf8 = "Volume"
        end
    end

    cairo_text_extents(cr, utf8, extents)
    local x = (conky_window.width - extents.width) / 2
    local y = bar_y - 16

    cairo_move_to (cr, x, y)
    cairo_show_text (cr, utf8)
end

function conky_main ()
    if conky_window == nil then
        return
    end
    update_state ()
    if opacity  <= 0 then
	    os.exit()
    end

    local cs = cairo_xlib_surface_create (conky_window.display,
                                         conky_window.drawable,
                                         conky_window.visual,
                                         conky_window.width,
                                         conky_window.height)
    cr = cairo_create (cs)

    draw (cr)

    cairo_destroy (cr)
    cairo_surface_destroy (cs)
    cr = nil
end
