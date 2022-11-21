;; estrutura de dados para comparacoes
(defstruct comps_struct
    comp_hor
    comp_ver
)

;; funcao para chamar no inicio, checa se é possivel completar o sudoku por meio de backtracking
;; retorna uma tupla com se foi possivel completar e o resultado obtido
(defun solveSudoku (grid comps)
    (setq done
        (make-result
            :done T
            :grid grid
        ))
    (setq pos (isSolved grid))
    (setq done_flag (2d_pos-x pos))
    (cond 
        ((= done_flag -1) done)
        (t (try grid comps pos 1))
    )
)
;; funcao que testa todos os números de 1 .. 9 na posição ecolhida
;; caso seja valido colocar o numero nessa posição, continua o backtracking
;; com um novo tabuleiro modificado
;; caso não seja retorna do backtracking e tenta seu antecessor
(defun try (grid comps pos num)
    (setq impossible
        (make-result
            :done NIL
            :grid grid
        ))

    (cond
        ((> num 9) impossible)
        (t (tryAs grid comps pos num))
    )
)
(defun tryAs (grid comps pos num)
    (setq validity (test grid comps pos num))
    (setq num2 (+ num 1))
    (cond
        (validity (tryLisp grid comps pos num))
        (t (try grid comps pos num2))
    )
)
(defun tryLisp (grid comps pos num)
    (setq grid2 (changeCell grid pos num))
    (setq result (solveSudoku grid2 comps))
    (setq solved (result-done result))
    (setq gridfinal (result-grid result))
    (setq num2 (+ num 1))
    (cond 
        (solved result)
        (t (try grid comps pos num2))
    )
)

;; estrutura de dados para resultado
(defstruct result
    done
    grid
)
;; testa se posição é valido colocar número nessa posição
(defun test (grid comps pos num)
    (and (checkColumn grid pos num) (checkLine grid pos num) (checkSquare grid pos num) (checkComp grid comps pos num))
)
;; -----------------------------------------------------

;; testa se nenhum numero que estamos tentando inserir ja esta na fila
(defun checkLine (grid pos num)
    (setq gridRow (getRow grid (2d_pos-x pos)))
    (checkLineAs gridRow num)
)
(defun checkLineAs (row num)
    (cond
        ((null row) T)
        ((= (car row) num) NIL)
        (t (checkLineAs (cdr row) num))
    )
)
;; -----------------------------------------------------

;; testa se nenhum numero que estamos tentando inserir ja esta na coluna
(defun checkColumn (grid pos num)
    (setq listColumn (getListColumn grid (2d_pos-y pos)))
    (checkLineAs listColumn num)
)
(defun getListColumn (grid column)
    (setq head (car grid))
    (setq item (cons (getRow head column) '()))
    (cond
        ((null grid) NIL)
        (t (append item (getListColumn (cdr grid) column)))
    )
)
;; -----------------------------------------------------

;; testa se o número ja esta presente no seu respectivo
;; quadrado 3x3
(defun checkSquare (grid pos num)
    (setq coordinates (getSquareCoods pos))
    (setq listSquare (getListSquare grid coordinates))
    (checkLineAs listSquare num)
)
(defun getListSquare (grid pos)
    (setq row (2d_pos-x pos))
    (setq row2 (+ row 1))
    (setq row3 (+ row 2))
    (setq column (2d_pos-y pos))
    (setq column2 (+ column 2))
    (defun sliceSquare (a)
        (slice column column2 (getRow grid a))
    )
    (append (sliceSquare row) (sliceSquare row2) (sliceSquare row3))
)

(defun getSquareCoods (pos)
    (setq row (2d_pos-x pos))
    (setq column (2d_pos-y pos))
    (setq srow (- row (mod row 3)))
    (setq scolumn (- column (mod column 3)))
    (make-2d_pos
        :x srow
        :y scolumn
    )
)
;; -----------------------------------------------------

;; testa se o número está de acordo com as comparações
(defun checkComp (grid comps pos num)
    (setq row (2d_pos-x pos))
    (setq column (2d_pos-y pos))
    (setq neighboors (getNeighboors grid pos))
    (defun compare (pos2)
        (defun subcompare (posn)
            (setq n_row (2d_pos-x posn))
            (setq n_column (2d_pos-y posn))
            (defun finalCompare (l r op)
                (cond
                    ((CHAR= op #\<) (< l r))
                    (t (> l r))
                )
            )
            (setq n_num (getRow (getRow grid n_row) n_column))
            (setq operator (getOperator pos posn comps))
            (finalCompare n_num num operator)
        )
        (if (null pos2)
            T
            (subcompare pos2)
        )
    )
    (setq maped (map (function compare) neighboors))
    (defun andList (iter)
        (cond
            ((null iter) T)
            ((car iter) (andList (cdr iter)))
            (t NIL)
        )
    )
    (andList maped)
)

;; pega operador entre posições
(defun getOperator (pos1 pos2 comps)
    (setq row (2d_pos-x pos1))
    (setq n_row (2d_pos-x pos2))
    (setq comp_hor (comps_struct-comp_hor comps))
    (setq comp_ver (comps_struct-comp_ver comps))

    (defun getop (pos1 pos2 comps2)
        (setq r (2d_pos-x pos1))
        (setq c (2d_pos-y pos1))
        (setq nr (2d_pos-x pos2))
        (setq nc (2d_pos-y pos2))
        (defun getMin (a b)
            (cond
                ((< a b) a)
                (t b)
            )
        )
        ;; fazer o minimo entre coordenadas já basta para pegar a posição
        ;; do comparador entre as duas posições
        (setq minr (getMin r nr))
        (setq minc (getMin c nc))
        (getRow (getRow comps2 minr) minc)
    )
    (cond
        ((= row n_row) (getop pos1 pos2 comp_hor))
        (t (getop pos1 pos2 comp_ver))
    )
)

;; pega vizinhos ortoganais das posições para realizar as comparações
(defun getNeighboors (grid pos)
    (append (getNeighboorsLast grid pos) (getNeighboorsNext grid pos))
)

(defun getNeighboorsLast (grid pos)
    (setq row (2d_pos-x pos))
    (setq column (2d_pos-y pos))
    (setq relativeRow (- (mod row 3) 1))
    (setq relativeColumn (- (mod column 3) 1))
    (setq column_n
        (make-2d_pos
            :x (- row 1)
            :y column
        ))
    (setq row_n
        (make-2d_pos
            :x row
            :y (- column 1)
        ))
    (setq return3 (cons column_n '())) 
    (setq return2 (cons row_n '())) 
    (setq return1 (append return3 return2)) 
    (cond
        ((and (>= relativeRow 0) (>= relativeColumn 0)) return1)
        ((>= relativeColumn 0) return2)
        ((>= relativeRow 0) return3)
        (t '())
    )
)

(defun getNeighboorsNext (grid pos)
    (setq row (2d_pos-x pos))
    (setq column (2d_pos-y pos))
    (setq relativeRow (+ (mod row 3) 1))
    (setq relativeColumn (+ (mod column 3) 1))
    (setq column_n
        (make-2d_pos
            :x (+ row 1)
            :y column
        ))
    (setq row_n
        (make-2d_pos
            :x row
            :y (+ column 1)
        ))
    (setq n_row_n (getRow (getRow grid row) (+ column 1)))       
    (setq n_column_n (getRow (getRow grid (+ row 1)) column))       
    (setq validRowN (and (< relativeColumn 3) (/= n_row_n 0)))
    (setq validColumnN (and (< relativeRow 3) (/= n_column_n 0)))

    (setq return3 (cons column_n '())) 
    (setq return2 (cons row_n '())) 
    (setq return1 (append return3 return2)) 
    (cond
        ((and validRowN validColumnN) return1)
        (validRowN return2)
        (validColumnN return3)
        (t '())
    )
)
;; -----------------------------------------------------

;; implementando funcoes take e drop do haskell
;; implementacao de slice
(defun take (iter num)
    (setq num2 (- num 1))
    (setq item (car iter))
    (setq to_insert (cons item '()))
    (cond
        ((null iter) '())
        ((> num 0) (append to_insert (take (cdr iter) num2)))
        (t '())
    )
)
(defun drop (iter num)
    (setq num2 (- num 1))
    (setq item (car iter))
    (setq to_insert (cons item '()))
    (cond
        ((null iter) '())
        ((<= num 0) (append to_insert (drop (cdr iter) num2)))
        (t (drop (cdr iter) num2))
    )
)
(defun slice (from to iter)
    (setq difference (- to from))
    (take (drop iter from) (+ difference 1))
)
;; -----------------------------------------------------

;; retorna uma linha da matriz apartir de um indice
(defun getRow (grid index)
    (getRowAs grid index 0)
)
(defun getRowAs (grid index cnt)
    (setq cnt2 (+ cnt 1))
    (cond
        ((= cnt index) (car grid))
        (t (getRowAs (cdr grid) index cnt2))
    )
)
;; -----------------------------------------------------

;; ;; funcao utilizada para saber se o sudoku já foi resolvido
;; ;; caso estaja completo retorna (-1, -1)
;; ;; caso não esteja retorna a coordenada do 0 encontrado
(defun isSolved (grid)
    (setq columns (map (function getcolumn) grid))
    (getPos columns 0)
)
(defun ehpar (n)
    (= (mod n 2) 0)
)
;; estrutura de dados para posição 2d
(defstruct 2d_pos
    x
    y
)
;; ;; map
(defun map (f list)
    (setq result (funcall f (car list)))
    (cond
        ((null list) ())
        (t (cons result (map f (cdr list))))
    )
)
;; Pega uma lista de int e retorna a primeira posição = 0
;; caso todas sejam zero retorna -1
(defun getColumn (row)
    (getColumnAs row 0)
)
(defun getColumnAs (row cnt)
    (setq cnt2 (+ cnt 1))
    (cond
        ((null row) -1)
        ((= 0 (car row)) cnt)
        (t (getColumnAs (cdr row) cnt2))
    )
)
;; mesma coisa que getColumn mas devolve uma tupla
(defun getPos (results cnt)
    (setq cnt2 (+ cnt 1))
    (setq head (car results))
    (setq body (cdr results))
    (setq full 
        (make-2d_pos
            :x -1
            :y -1
        ))
    (setq pos
        (make-2d_pos
            :x cnt
            :y head
        ))
    (cond
        ((null results) full)
        ((/= head -1) pos)
        (t (getPos body cnt2))
    )
)
;; ------------------------------------------------
;; cria uma nova matriz setando o valor estipulado na posição para o numero recebido
(defun changeCell (grid pos num)
    (setq row (2d_pos-x pos))
    (setq column (2d_pos-y pos))
    (setq row2 (+ row 1))
    (setq column2 (+ column 1))

    (setq before (take grid row))
    (setq after (drop grid row2))

    (setq gridRow (getRow grid row))

    (setq beforeRow (take gridRow column))
    (setq afterRow (drop gridRow column2))
    (setq numAppend (cons num '()))

    (setq newRow (append beforeRow numAppend afterRow))
    (setq newRowAppend (cons newRow '()))

    (append before newRowAppend after)
)

