
// Constants
const redThrone = [4,2];
const blueThrone = [0,2];
const blueMaster = "BM";
const redMaster = "RM";
const bluePawn = "BP";
const redPawn = "RP";

// create board
var b = jsboard.board({attach:"game", size:"5x5"});
b.cell("each").style({width:"75px", height:"75px"});

// setup pieces
var piece_red_master    = jsboard.piece({text:redMaster, textIndent:"-9999px", background:"url('images/white.png') no-repeat", width:"50px", height:"50px", margin:"0 auto", color:"red"});
var piece_red_pawn      = jsboard.piece({text:redPawn, textIndent:"-9999px", background:"url('images/red.png') no-repeat", width:"50px", height:"50px", margin:"0 auto", color:"red" });
var piece_blue_master   = jsboard.piece({text:blueMaster, textIndent:"-9999px", background:"url('images/bknight.png') no-repeat", width:"50px", height:"50px", margin:"0 auto", color:"blue" });
var piece_blue_pawn     = jsboard.piece({text:bluePawn, textIndent:"-9999px", background:"url('images/black.png') no-repeat", width:"50px", height:"50px", margin:"0 auto", color:"blue" });

// create pieces to place in DOM
var pieces = [
    piece_red_master.clone(),
    piece_red_pawn.clone(),
    piece_red_pawn.clone(),
    piece_red_pawn.clone(),
    piece_red_pawn.clone(),
    piece_blue_master.clone(),
    piece_blue_pawn.clone(),
    piece_blue_pawn.clone(),
    piece_blue_pawn.clone(),
    piece_blue_pawn.clone()
];

// place pieces on board
b.cell([4,2]).place(pieces[0]);
b.cell([4,0]).place(pieces[1]);
b.cell([4,1]).place(pieces[2]);
b.cell([4,3]).place(pieces[3]);
b.cell([4,4]).place(pieces[4]);
b.cell([0,2]).place(pieces[5]);
b.cell([0,0]).place(pieces[6]);
b.cell([0,1]).place(pieces[7]);
b.cell([0,3]).place(pieces[8]);
b.cell([0,4]).place(pieces[9]);

//Cards
var cards = [ 
  ["horse",     [1, 0], [0, -1], [-1, 0]],

  ["ox",        [1, 0], [-1, 0], [0, 1]],

  ["crane",     [1, 0], [-1, -1], [-1, 1]],

  ["mantis",    [1, -1], [1, 1], [-1, 0]],
    
  ["eel",       [1, -1], [-1, -1], [0, 1]],

  ["cobra",     [0, -1], [1, 1], [-1, 1]],

  ["rooster",   [-1, -1], [0, -1], [0, 1], [1, 1]],
             
  ["goose",     [-1, 1], [0, -1], [0, 1], [1, -1]],

  ["frog",      [0, -2], [1, -1], [-1, 1]],
 
  ["rabbit",    [-1, -1], [1, 1], [0, 2]],

  ["monkey",    [1, -1], [1, 1], [-1, -1], [-1, 1]],

  ["boar",      [0, 1], [0, -1], [1, 0]],

  ["tiger",     [2, 0], [-1, 0]],

  ["dragon",    [-1, -1], [-1, 1], [1, -2], [1, 2]],

  ["crab",      [1, 0], [0, -2], [0, 2]],

  ["elephant",  [1, -1], [0, -1], [0, 1], [1, 1]]
]

var moveTracker = true;
var firstCell = "";
var secondCell = "";
var move = "";
var newLocs = "";

var redTurn = true;
var gameOver = false;

var candidateMoves = [];

var cellListener = function(cell) { firstCellListener(cell);};

b.cell("each").on("click", function() { cellListener(b.cell(this)); });

function firstCellListener(cell) {
    highlight_candidate_cells(false);
    console.log("first text");
    
    if (gameOver) return;

        if (cell_not_empty(cell)) {
            firstCell = cell;
            if (!check_turn()) {
                console.log("Not this piece's turn");
            }
            else {
                highlight_cell(true, firstCell);
                console.log("Start", firstCell);
                                                                        //placeholder
                candidateMoves = get_candidate_moves(firstCell, ["dragon",    [-1, -1], [-1, 1], [1, -2], [1, 2]]); 
                highlight_candidate_cells(true);
    
                cellListener = secondCellListener;
            }
        }
    
}

function secondCellListener(cell) {
    console.log("second text");
    
    secondCell = cell;
        console.log("Second cell:", secondCell.get());
    
            if (friendly_piece(firstCell, secondCell)) {
                highlight_cell(false, firstCell);
                firstCellListener(secondCell);
            }
    
            else if (is_candidate_move(secondCell.where())) {
	           console.log("End:", secondCell);
        
                highlight_cell(false, firstCell);
                highlight_candidate_cells(false);
                game_over();
                move_piece();
                redTurn = !redTurn;
                
                cellListener = firstCellListener;
            }
}

function friendly_piece(firstCell, secondCell) {
  return (cell_not_empty(secondCell) && (secondCell.get().charAt(0) == firstCell.get().charAt(0))); 
}

function same_piece() {
    return (cell_not_empty(secondCell) && (secondCell.get().toString() == firstCell.get().toString()));
}

function piece_color() {
    return firstCell.get().charAt(0);   
}

function move_piece() {
	secondCell.place(firstCell.DOM().children[0]);
}

function highlight_cell(highlight, cell) {
    if (highlight){
       cell.DOM().classList.add("green");
    }
    else {
        cell.DOM().classList.remove("green"); 
    }
}

function which_turn() {
 if (redTurn) {
     return 'R';
 }
    else {
        return 'B';
    }
}

function check_turn() {
 return (piece_color() == which_turn());   
}

function game_over() {
    gameOver = (
      opposite_master(secondCell) ||
      opposite_throne(secondCell) && my_master(firstCell)
    ) 
}

function opposite_master(cell) {
    if (redTurn) {
        return cell.get() == blueMaster;
    }
    else {
        return cell.get() == redMaster;   
    }
}

function opposite_throne(cell) {
    if (redTurn) {
        return compare_coordinates(cell.where(), blueThrone);
    }
    else {
        return compare_coordinates(cell.where(), redThrone);
    }
}

function my_master(cell) {
    if (redTurn) {
        return cell.get() == redMaster;
    }
    else {
        return cell.get() == blueMaster;
    }
}

function cell_not_empty(cell) {
    return cell.get() != null;
}

function compare_coordinates(cell_1, cell_2) {
    return cell_1.toString() == cell_2.toString();   
}

function randomize_num_of_moves(min_range, max_range) {
    return Math.floor(Math.random() * (max_range - min_range + 1) ) + min_range;
}

function randomize_coordinate() {
    return Math.floor(Math.random() * 5);
}

function randomize_move() {
    return [randomize_coordinate(), randomize_coordinate()];
}

function get_candidate_moves(piece, card) {
    
    var x = 0;
    var y = 0;
    var moveList = [];
    var direction;

    if (redTurn) {
	    direction = -1;
    }
    else
    {
	    direction = 1;
    }	    
    
    //generate moves here()
    //starts with one because first item in cards list is name of card
    var rules  = card.slice(1); 
    var piece_coords = piece.where();
    for (i = 0; i < rules.length; i++) {
	var up_down = rules[i][0];
        var left_right = rules[i][1];	
        move = [(up_down * direction) + piece_coords[0], (left_right * direction) + piece_coords[1]];
        console.log("MOVE", move);
        
        if (!friendly_piece(piece, b.cell(move)) && within_cell_range(move))
        {
            moveList.push(move);
            console.log("MOVELIST (AT i:", i, "):", moveList);
             
        }
        
    }

    console.log("MOVELIST: ", moveList.toString());
    return moveList;
    
}

function within_cell_range(move){
    if (move[0] >= 0 && move[0] < 5 && move[1] >= 0 && move[1] < 5) {
        return true
    }
}

function is_candidate_move(move) {
    console.log("is candidate move", move);
    for (i = 0; i < candidateMoves.length; i++) {
        console.log("highlighed", i, candidateMoves[i]);
        if (compare_coordinates(candidateMoves[i], move)) {
            return true;
        }
    }	    
    return false;
    
}

//for cells, highlight true
//unhighlight other cells
function highlight_candidate_cells(highlight) {
    
    for (i = 0; i < candidateMoves.length; i++)
    {
        highlight_cell(highlight, b.cell(candidateMoves[i]));
    }
}

//Change the direction of the moves depending on which player's turn it is
//FUNCTION NOT IN USE CURRENTLY
function change_direction(is_redTurn, card) {
    //Multiply moves by -1
    var rules  = card.slice(1);
    
    var inverted_list = [];
    
    //if player 1 turn, direction = -1
    if (is_redTurn) //redTurn
    {
        
        for (i = 0; i < rules.length; i++) 
        {
	        var up_down = rules[i][0];
            var left_right = rules[i][1];
            var move = [up_down * redDirection, left_right * redDirection]
            
            inverted_list.push(move);
            console.log("RED MOVES (AT i:", i, "):", inverted_list);
        }
        
        return inverted_list;
        
    }
    
    
}

function send_to_server(move) {
	console.log("Move", move);
}
