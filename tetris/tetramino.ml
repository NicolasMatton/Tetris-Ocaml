let n = Random.self_init()

type kind =
	| Barre
	| Lr
	| Ll
	| Square
	| T
	| Zr
	| Zl

let barreI=
	[(6,0);(6,1);(6,2);(6,3)]
	
let lrI=
	[(6,0);(6,1);(6,2);(7,2)]

let llI=
	[(7,0);(7,1);(7,2);(6,2)]

let squareI=
	[(6,0);(6,1);(7,0);(7,1)]

let tI=
	[(7,0);(7,1);(6,1);(8,1)]

let zrI=
	[(6,1);(7,1);(7,0);(8,0)]

let zlI=
	[(6,0);(7,0);(7,1);(8,1)]
	
type tetramino = {
  k : kind;
  mutable squares : (int * int) list
}

let makeT (tetra:kind) =
  match tetra with
  | Barre -> 	{ k = tetra; squares = barreI }
  | Lr -> 	{ k = tetra; squares = lrI }
  | Ll -> 	{ k = tetra; squares = llI }
  | Square -> 	{ k = tetra; squares = squareI }
  | T -> 	{ k = tetra; squares = tI }
  | Zr -> 	{ k = tetra; squares = zrI }
  | Zl -> 	{ k = tetra; squares = zlI }

let make_random () =
  let r = (Random.int 6) in
  match r with
  |0->makeT Barre
  |1->makeT Lr
  |2->makeT Ll
  |3->makeT T
  |4->makeT Zr
  |5->makeT Square
  |6->makeT Zl
  |_->raise(Invalid_argument "inutil")

	
let down (token : tetramino)=
  token.squares<-List.map (fun (a,b)->(a,b+1)) token.squares
	   
let left (token : tetramino)=
	let test = ref false in
	for k=0 to 3 do
		let (a,b)= (List.nth token.squares k)in
		if a = 0 then test:=true
	done;
	if(not !test) then token.squares<-List.map(fun (a,b)->(a - 1,b)) token.squares

let right (token : tetramino)=
  let test = ref false in
	for k=0 to 3 do
		let (a,b)= (List.nth token.squares k)in
		if a = 13 then test:=true
	done; 
	if(not !test) then token.squares<-List.map(fun (a,b)->(a + 1,b)) token.squares


	


