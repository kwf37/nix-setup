-- This is a lua script for use in Conky.
require 'cairo'

-- Variables for tracking brightness changes
local brightness = conky_parse("${exec light -G}")
local prev_brightness = brightness
local stale_counter = 0
local opacity = 1.0

function update_brightness ()
    prev_brightness = brightness
    brightness = conky_parse("${exec light -G}")
    if brightness == prev_brightness then
        stale_counter = stale_counter + 1
    else
        stale_counter = 0
        opacity = 1.0
    end

    if stale_counter > 50 and opacity > 0 then opacity = opacity - 0.05
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
    bar_percent = conky_parse("${exec light -G}")
    bar_max_width = 300
    bar_width = bar_max_width * tonumber(bar_percent) / 100
    bar_height = 15 
    bar_x = (conky_window.width - bar_max_width) / 2
    bar_y = conky_window.height - (bar_height + 20)
    bar_rgba = {0.8, 0.8, 0.8, opacity}

    cairo_set_source_rgba (cr, bar_rgba[1], bar_rgba[2], bar_rgba[3], bar_rgba[4])
    cairo_rectangle(cr, bar_x, bar_y, bar_width, bar_height)
    cairo_fill(cr)

    -- Draw Brightness Label
    cairo_select_font_face (cr, "DejaVu Sans Mono", CAIRO_FONT_SLANT_NORMAL,
                               CAIRO_FONT_WEIGHT_BOLD);
    cairo_set_font_size (cr, 20);
    local extents = cairo_text_extents_t:create()
    local utf8 = "Brightness"

    cairo_text_extents(cr, utf8, extents)
    local x = extents.width
    local y = bar_y - 16

    cairo_move_to (cr, x, y)
    cairo_show_text (cr, utf8)
end

function conky_main ()
    if conky_window == nil then
        return
    end
    local cs = cairo_xlib_surface_create (conky_window.display,
                                         conky_window.drawable,
                                         conky_window.visual,
                                         conky_window.width,
                                         conky_window.height)
    cr = cairo_create (cs)
    update_brightness ()
    draw (cr)
    local updates = tonumber (conky_parse ('${updates}'))
    if updates > 5 then
        -- print ("conky_main counted >5 updates to its window")
        -- print (conky_parse ('${updates}'))
    end
    cairo_destroy (cr)
    cairo_surface_destroy (cs)
    cr = nil
end