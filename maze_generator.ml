open Stdlib
open Bigarray

module Maze = struct
    let bottom_wall_bit = 0b00000001
    and right_wall_bit  = 0b00000010
    and visited_bit     = 0b00000100

    type t = {
        width: int;
        height: int;
        data: (int, int8_unsigned_elt, c_layout) Array2.t;
    }
    type direction = Right | Bottom | Left | Top

    let v width height =
        {
            width = width;
            height = height;
            data = Array2.init Int8_unsigned c_layout width height (fun _ _ -> bottom_wall_bit lor right_wall_bit)
        }

    let apply_direction (x, y) = function
        | Right  -> (x + 1, y)
        | Bottom -> (x, y + 1)
        | Left   -> (x - 1, y)
        | Top    -> (x, y - 1)

    let is_direction_possible maze (x, y) dir =
        let (ax, ay) = apply_direction (x, y) dir in

        ax < maze.width
            && ay < maze.height
            && ax >= 0
            && ay >= 0
            && (maze.data.{ax, ay} land visited_bit) = 0

    let get_possible_directions maze pos =
        List.filter (is_direction_possible maze pos) [Right; Bottom; Left; Top]

    let generate_maze maze = begin
        Random.self_init ();

        let visit_cell_and_break_wall (cx, cy) dir =
            let (ax, ay) = apply_direction (cx, cy) dir in
            (match dir with
                | Right  -> maze.data.{cx, cy} <- maze.data.{cx, cy} land (lnot right_wall_bit)
                | Bottom -> maze.data.{cx, cy} <- maze.data.{cx, cy} land (lnot bottom_wall_bit)
                | Left   -> maze.data.{ax, ay} <- maze.data.{ax, ay} land (lnot right_wall_bit)
                | Top    -> maze.data.{ax, ay} <- maze.data.{ax, ay} land (lnot bottom_wall_bit)
            );

            (ax, ay)
        in

        let rec gen_step cursor_pos tail =
            let possible_directions = get_possible_directions maze cursor_pos in
            (
                let (cx, cy) = cursor_pos in
                    maze.data.{cx, cy} <- maze.data.{cx, cy} lor visited_bit
            );

            match possible_directions with
                | [] -> (
                    match tail with
                    | [] -> ()
                    | new_pos :: tail -> gen_step new_pos tail
                )
                | _ -> begin
                    let dir = List.nth possible_directions (Random.int (List.length possible_directions)) in
                    let new_cursor = visit_cell_and_break_wall cursor_pos dir in
                    
                    
                    gen_step new_cursor (new_cursor :: tail)
                end

        in gen_step (0, 0) [];
    end

    let image_of_maze maze = begin
        let open Bimage in

        let image = Image.v u8 rgb (1 + maze.width * 2) (1 + maze.height * 2) in

        let set_pixel x y color = Image.set_pixel image x y (Pixel.v Color.rgb color) in
        let set_pixel_black x y = set_pixel x y [0.; 0.; 0.] in
        
        Image.for_each (fun x y _ -> set_pixel x y [1.; 1.; 1.]) image;

        set_pixel_black 0 0;
        for x = 0 to (maze.width - 1) do
            set_pixel_black (x * 2 + 1) 0;
            set_pixel_black (x * 2 + 2) 0;
            for y = 0 to (maze.height - 1) do
                set_pixel_black 0 (y * 2 + 1);
                set_pixel_black 0 (y * 2 + 2);

                set_pixel_black (x * 2 + 2) (y * 2 + 2);
                if maze.data.{x, y} land right_wall_bit <> 0 then
                    set_pixel_black (x * 2 + 2) (y * 2 + 1);
                if maze.data.{x, y} land bottom_wall_bit <> 0 then
                    set_pixel_black (x * 2 + 1) (y * 2 + 2);
            done
        done;

        image
    end

end

let _ = 
    let open Bimage_unix in

    let maze = Maze.v 100 100 in
    Maze.generate_maze maze;
    let image = Maze.image_of_maze maze in 
    
    
    Magick.write "output.png" image;
