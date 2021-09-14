open Stdlib
open Bigarray

(* Module Containing every maze specific type, values and methods *)
module Maze = struct
    (* Definitions for the 8-Bit cell bitfield *)
    (** Bit defined if the bottom wall of the cell is present *)
    let bottom_wall_bit = 0b00000001
    (** Bit defined if the right wall of the cell is present *)
    and right_wall_bit  = 0b00000010
    (** Bit defined if the cell has been visited by the generation cursor *)
    and visited_bit     = 0b00000100

    (* Type of maze (following convention called t) *)
    (** Maze type *)
    type t = {
        width: int;
        height: int;
        data: (int, int8_unsigned_elt, c_layout) Array2.t;
    }

    (** Enum type used for representing a direction *)
    type direction = Right | Bottom | Left | Top
    (* Constructor following naming convention *)
    (** Create a new maze *)
    let v width height =
        {
            width = width;
            height = height;
            data = Array2.init Int8_unsigned c_layout width height (fun _ _ -> bottom_wall_bit lor right_wall_bit)
        }

    (** Applies a direction to a position tuple *)
    let apply_direction (x, y) = function
        | Right  -> (x + 1, y)
        | Bottom -> (x, y + 1)
        | Left   -> (x - 1, y)
        | Top    -> (x, y - 1)

    (** Returns true if the movement of direction from position in the maze is possible for the generation cursor
        i.e. True if the destination position is contained inside the maze and isn't visited
     *)
    let is_direction_possible maze (x, y) dir =
        let (ax, ay) = apply_direction (x, y) dir in

        ax < maze.width
            && ay < maze.height
            && ax >= 0
            && ay >= 0
            (* The cell bitfield must not be already visited *)
            && (maze.data.{ax, ay} land visited_bit) = 0

    (** Return a list of possible all possible direction the cursor can take (based on [is_direction_possible] *)
    let get_possible_directions maze pos =
        (* filter the list of all directions with is_direction_possible *)
        List.filter (is_direction_possible maze pos) [Right; Bottom; Left; Top]

    (** Generate the maze with the cursor starting a (0, 0) *)
    let generate_maze maze = begin
        (* Randomize the RNG (because why not) *)
        Random.self_init ();

        (* Untility function to make the cursor move by removing the wall in its passage *)
        let move_cursor (cx, cy) dir =
            let (ax, ay) = apply_direction (cx, cy) dir in
            (match dir with
                | Right  -> maze.data.{cx, cy} <- maze.data.{cx, cy} land (lnot right_wall_bit)
                | Bottom -> maze.data.{cx, cy} <- maze.data.{cx, cy} land (lnot bottom_wall_bit)
                | Left   -> maze.data.{ax, ay} <- maze.data.{ax, ay} land (lnot right_wall_bit)
                | Top    -> maze.data.{ax, ay} <- maze.data.{ax, ay} land (lnot bottom_wall_bit)
            );

            (ax, ay)
        in

        (*
            Recursive function for looping until the generation is finished
            cursor_pos is the cursor current postion
            tail       is a List containing all previous position the cursor was for backtracing
        *)
        let rec gen_step cursor_pos tail =
            (* Mark the cursor current position as visited *)
            (
                let (cx, cy) = cursor_pos in
                    maze.data.{cx, cy} <- maze.data.{cx, cy} lor visited_bit
            );

            (* Check for potential cursor movements *)
            match ( get_possible_directions maze cursor_pos ) with
                (* If no movement is possible *)
                | [] -> (
                    match tail with
                    (* And no backtracing (tail is empty) is possible either then stop the generation *)
                    | [] -> ()
                    (*
                        If backtracing possible, take the last position from the tail
                        and continue generation from there
                    *)
                    | new_pos :: tail -> gen_step new_pos tail
                )
                (* If movement is possible *)
                | possible_directions -> begin
                    (* Choose randomly a direction from the list *)
                    let dir = List.nth possible_directions (Random.int (List.length possible_directions)) in
                    (* And move the cursor *)
                    let new_cursor = move_cursor cursor_pos dir in
                    (* And continue generation, with the new position added to the tail *)
                    gen_step new_cursor (new_cursor :: tail)
                end

        (* Start the iteration with an empty tail and cursor a (0, 0) *)
        in gen_step (0, 0) [];
    end

    (** Convert the maze into a black and white image (white for traversable, black for a wall) *)
    let image_of_maze maze = begin
        (* Open the image library for clarity *)
        let open Bimage in

        (* 
            Create an image of twice the maze size
            Because each cell will be 4 pixels²
            (interior, right wall, bottom wall and corner wall)
            In ascii art:
               n#
               ##
            n being the interior and # walls
        *)
        let image = Image.v u8 rgb (1 + maze.width * 2) (1 + maze.height * 2) in

        (* Shortcut functions *)
        let set_pixel x y color = Image.set_pixel image x y (Pixel.v Color.rgb color) in
        let set_pixel_black x y = set_pixel x y [0.; 0.; 0.] in
        
        (* Setting all pixels to white *)
        Image.for_each (fun x y _ -> set_pixel x y [1.; 1.; 1.]) image;

        (* Setting the top left corner black *)
        set_pixel_black 0 0;
        (* For each maze cell *)
        for x = 0 to (maze.width - 1) do
            (* 
               Using the loop to set the top and left border of the image
               Because the cells aren't using these, they would be left white which we don't want
            *)
            set_pixel_black (x * 2 + 1) 0;
            set_pixel_black (x * 2 + 2) 0;
            for y = 0 to (maze.height - 1) do
                (* Same as before *)
                set_pixel_black 0 (y * 2 + 1);
                set_pixel_black 0 (y * 2 + 2);

                (* Setting the cell corner wall to black *)
                set_pixel_black (x * 2 + 2) (y * 2 + 2);
                (* Checking right and bottom wall for conditionally settings the pixels *)
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
