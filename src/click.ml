open Game
open Card
open Table
open Player
open Drawing
open State
open Command
open Graphics

type event =
  | Button_down
  | Button_up
  | Key_pressed
  | Mouse_motion
  | Poll

type status = {
  mouse_x : int;
  mouse_y : int;
  button : bool;
  keypressed : bool;
  key : char;
}

let select_card = 
  Graphics.set_color yellow; (** grey*)
  Graphics.fill_rect 
  
let t_init s () =
  Graphics.open_graph
    (" "
    ^ string_of_int (s.scale * s.maxx)
    ^ "x"
    ^ string_of_int (s.scale * s.maxy));
  Graphics.set_color s.bc;
  Graphics.fill_rect 0 0
    ((s.scale * s.maxx) + 1)
    ((s.scale * s.maxy) + 1);
  draw_point s.x s.y s.scale s.pc
