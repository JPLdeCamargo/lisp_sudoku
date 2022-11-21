;; funcao para chamar no inicio, checa se é possivel completar o sudoku por meio de backtracking
;; retorna uma tupla com se foi possivel completar e o resultado obtido
(defun solveSudoku (grid)
    (setq done
        (make-result
            :done T
            :grid grid
        ))
    (setq pos (isSolved grid))
    (setq done_flag (2d_pos-x pos))
    (cond 
        ((= done_flag -1) done)
        (t (try grid pos 1))
    )
)
;; funcao que testa todos os números de 1 .. 9 na posição ecolhida
;; caso seja valido colocar o numero nessa posição, continua o backtracking
;; com um novo tabuleiro modificado
;; caso não seja retorna do backtracking e tenta seu antecessor
(defun try (grid pos num)
    (setq impossible
        (make-result
            :done NIL
            :grid grid
        ))

    (cond
        ((> num 9) impossible)
        (t (tryAs grid pos num))
    )
)
(defun tryAs (grid pos num)
    (setq validity (test grid pos num))
    (setq num2 (+ num 1))
    (cond
        (validity (tryLisp grid pos num))
        (t (try grid pos num2))
    )
)
(defun tryLisp (grid pos num)
    (setq grid2 (changeCell grid pos num))
    (setq result (solveSudoku grid2))
    (setq solved (result-done result))
    (setq gridfinal (result-grid result))
    (setq num2 (+ num 1))
    (cond 
        (solved result)
        (t (try grid pos num2))
    )
)

;; estrutura de dados para resultado
(defstruct result
    done
    grid
)
;; testa se posição é valido colocar número nessa posição
(defun test (grid pos num)
    (and (checkColumn grid pos num) (checkLine grid pos num) (checkSquare grid pos num))
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

