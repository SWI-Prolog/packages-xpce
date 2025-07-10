#include <stdbool.h>
#define COMMON(type) SO_LOCAL type

/* ../src/x11/xcolour.c */
COMMON(status)	ws_create_colour(Colour c, DisplayObj d);
COMMON(void)	ws_uncreate_colour(Colour c, DisplayObj d);
COMMON(status)	ws_colour_name(DisplayObj d, Name name);
COMMON(Colour)	ws_pixel_to_colour(DisplayObj d, unsigned long pixel);
COMMON(void)	ws_colour_cube(ColourMap cm, int size);
COMMON(void)	ws_colour_map_colours(ColourMap cm);
COMMON(status)	ws_create_colour_map(ColourMap cm, DisplayObj d);
COMMON(status)	ws_uncreate_colour_map(ColourMap cm, DisplayObj d);
COMMON(status)	ws_unlink_colour_map(ColourMap cm);

/* ../src/x11/xcursor.c */
COMMON(void)	ws_init_cursor_font(void);
COMMON(Int)	ws_cursor_font_index(Name name);
COMMON(status)	ws_create_cursor(CursorObj c, DisplayObj d);
COMMON(void)	ws_destroy_cursor(CursorObj c, DisplayObj d);

/* ../src/x11/xframe.c */
COMMON(bool)	ws_window_frame_position(Any window, FrameObj fr,
					 int *ox, int *oy);

COMMON(status)	ws_created_frame(FrameObj fr);
COMMON(void)	ws_uncreate_frame(FrameObj fr);
COMMON(status)	ws_create_frame(FrameObj fr);
COMMON(void)	ws_realise_frame(FrameObj fr);
COMMON(PceWindow)ws_window_holding_point_frame(FrameObj fr);
COMMON(void)	ws_raise_frame(FrameObj fr);
COMMON(void)	ws_lower_frame(FrameObj fr);
COMMON(status)	ws_attach_wm_prototols_frame(FrameObj fr);
COMMON(status)	setDndAwareFrame(FrameObj fr);
COMMON(void)	ws_grab_frame_pointer(FrameObj fr, BoolObj grab, CursorObj cursor);
COMMON(status)	ws_enable_text_input(Graphical gr, BoolObj enable);
COMMON(status)	ws_frame_bb(FrameObj fr, int *x, int *y, int *w, int *h);
COMMON(void)	ws_x_geometry_frame(FrameObj fr, Name spec, Monitor mon);
COMMON(void)	ws_geometry_frame(FrameObj fr, Int x, Int y, Int w, Int h, Monitor mon);
COMMON(void)	ws_border_frame(FrameObj fr, int b);
COMMON(void)	ws_busy_cursor_frame(FrameObj fr, CursorObj c);
COMMON(void)	ws_frame_cursor(FrameObj fr, CursorObj cursor);
COMMON(void)	ws_frame_background(FrameObj fr, Any c);
COMMON(void)	ws_set_icon_frame(FrameObj fr);
COMMON(void)	ws_set_icon_label_frame(FrameObj fr);
COMMON(void)	ws_set_icon_position_frame(FrameObj fr, int x, int y);
COMMON(status)	ws_get_icon_position_frame(FrameObj fr, int *x, int *y);
COMMON(void)	ws_enable_modal(FrameObj fr, BoolObj val);
COMMON(void)	ws_status_frame(FrameObj fr, Name status);
COMMON(void)	ws_topmost_frame(FrameObj fr, BoolObj topmost);
COMMON(void)	ws_set_label_frame(FrameObj fr);
COMMON(Image)	ws_image_of_frame(FrameObj fr);
COMMON(void)	ws_transient_frame(FrameObj fr, FrameObj fr2);
COMMON(status)	ws_postscript_frame(FrameObj fr, int iscolor);
COMMON(Int)	ws_frame_thread(FrameObj fr);
COMMON(int)	ws_enable_frame(FrameObj fr, int enable);

/* ../src/x11/xstream.c */
COMMON(void)	ws_close_input_stream(Stream s);
COMMON(void)	ws_close_output_stream(Stream s);
COMMON(void)	ws_close_stream(Stream s);
COMMON(void)	ws_input_stream(Stream s);
COMMON(void)	ws_no_input_stream(Stream s);
COMMON(void)	ws_listen_socket(Socket s);
COMMON(status)	ws_write_stream_data(Stream s, void *data, int len);
COMMON(int)	ws_read_stream_data(Stream s, void *data, int len, Real timeout);
COMMON(void)	ws_done_process(Process p);

/* ../src/x11/xtimer.c */
COMMON(void)	ws_status_timer(Timer tm, Name status);

/* ../src/x11/xwindow.c */
COMMON(status)	ws_created_window(PceWindow sw);
COMMON(void)	ws_uncreate_window(PceWindow sw);
COMMON(status)	ws_create_window(PceWindow sw, PceWindow parent);
COMMON(void)	ws_manage_window(PceWindow sw);
COMMON(void)	ws_unmanage_window(PceWindow sw);
COMMON(void)	ws_reassociate_ws_window(PceWindow from, PceWindow to);
COMMON(void)	ws_geometry_window(PceWindow sw, int x, int y, int w, int h, int pen);
COMMON(void)	ws_topmost_window(PceWindow sw, BoolObj topmost);
COMMON(void)	ws_grab_pointer_window(PceWindow sw, BoolObj val);
COMMON(void)	ws_grab_keyboard_window(PceWindow sw, BoolObj val);
COMMON(void)	ws_grab_pointer_window(PceWindow sw, BoolObj val);
COMMON(void)	ws_grab_keyboard_window(PceWindow sw, BoolObj val);
COMMON(void)	ws_ungrab_all(void);
COMMON(void)	ws_flash_area_window(PceWindow sw, int x, int y, int w, int h, int msecs);
COMMON(void)	ws_flash_window(PceWindow sw, int msecs);
COMMON(void)	ws_move_pointer(PceWindow sw, int x, int y);
COMMON(void)	ws_window_cursor(PceWindow sw, CursorObj cursor);
COMMON(void)	ws_window_background(PceWindow sw, Any c);
COMMON(void)	ws_raise_window(PceWindow sw);
COMMON(void)	ws_lower_window(PceWindow sw);
COMMON(int)	ws_enable_window(PceWindow sw, int enable);
COMMON(Int)	ws_window_thread(PceWindow sw);
COMMON(int)	ws_delayed_redraw_window(PceWindow sw);

/* ../src/x11/x11.c */
COMMON(void)	ws_initialise(int argc, char **argv);
COMMON(int)	ws_version(void);
COMMON(int)	ws_revision(void);
COMMON(status)	ws_show_console(Name how);
COMMON(status)	ws_console_label(CharArray label);
COMMON(Int)	ws_default_scrollbar_width(void);
COMMON(char *)	ws_user(void);

/* ../src/x11/xmenu.c */
COMMON(status)	ws_draw_scrollbar_arrow(ScrollBar s, int x, int y, int w, int h, Name which, int up);
COMMON(int)	ws_arrow_height_scrollbar(ScrollBar s);
COMMON(status)	ws_draw_sb_thumb(int x, int y, int w, int h);
COMMON(Colour)	ws_3d_grey(void);
COMMON(status)	ws_draw_button_face(DialogItem di, int x, int y, int w, int h, int up, int defb, int focus);
COMMON(int)	ws_combo_box_width(Graphical gr);
COMMON(int)	ws_stepper_width(Graphical gr);
COMMON(int)	ws_entry_field_margin(void);
COMMON(status)	ws_entry_field(Graphical gr, int x, int y, int w, int h, int flags);
COMMON(status)	ws_draw_checkbox(int x, int y, int w, int h, int b, int flags);
COMMON(status)	ws_checkbox_size(int flags, int *w, int *h);
COMMON(int)	ws_message_box(Any msg, int flags);

/* ../src/gra/graphstate.c */
COMMON(void)	g_save(void);
COMMON(void)	g_restore(void);
COMMON(int)	g_level(void);
