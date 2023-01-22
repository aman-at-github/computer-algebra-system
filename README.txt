# computer-algebra-system

Computer Algebra System


Can create Algebraic-Expressions like MATLAB in Command-Line.
Also allows programming on these objects of Algebraic-Expressions in Java.


Functionalities include : 
  Differential Equations 
  2D Matrices 
  Solving System of Consistent Overdetermined Linear-Equations 
  Source Code has a wide array of useful functions on processing 3D-Vectors and Matrices. 



Source Code - 
  'Equationsimplifier2.java' contains Sorce-Code, and when executed runs Command-Line Algebraic-Expressions like MATLAB.

Dependencies - 
  'gov.nist.math.jama-1.1.1.jar' is only used to solve Consistent System-Of-Linear-Equations and to calculate SVD of matrices.
  'org.json-chargebee-1.0.jar' is used for JSON functionality.




More Information : 
•	Capable of creating symbolic variables & algebraic-expressions from 'Text' and vice-versa. Allows manipulating them, perform algebraic-operations and programming on them.
•	Can determine Derivatives of algebraic-expressions upto any given degree. 
•	Handles symbolic-variables in Scalars, Vectors & Matrices. 
•	Can give Symbolic-Solution to a system-of linear-equations. 
•	Includes a wide array of advanced operators such as Matrix-operators, Vector-operators, Trigonometric, Logarithmic & Exponential operators.
•	Offers functionalities of Approximate-Equality, a very useful feature well integrated with other features.  (eg: '2.0004' is nearly-equal-to '2' as well as '1.9991').  
•	Gives numerical-solution for any consistent Over-Determined-System of Linear-Equations. (Another useful feature not offered by conventionally-available libraries to solve a System-of-Linear-Equations). 




RULES FOR COMMAND LINE  ALGEBRA : 

// Input Strings must not contain '\b' ie backspace characters 
    
    // MATRIX, ARRAY & OPERATOR Conventions : 
    
    // 1D-arrays and 2D-matrices are indicated using curly-braces 
    // 1d-array such as " { a , b , c , d } " is converted to " { { a } , { b } , { c } , { d } } " ie. stored as a matrix with multiple rows but single column , vector-element with index 'i' is stored at :  expr.expr_arr2d_matrix_or_vector_operands[ i ][ 0 ] 
    // all opening and closing brackets must be preceeded by and followed by a blank-space-character 
    
    // most operators begin with '$'  eg $PRD_( a ; b ; c ) 
    // use of brackets per operator is compulsary 
        // ( a + b + c )  is NOT-VALID 
        // ( ( a + b ) + c )  is VALID 
        // $SUM_( a + b + c )  is VALID 
        // $SUM_( a , b , c )  is VALID 
        // $PRD_( a * b * c )  is VALID 
    // use of brackets per operator is mandatory except cases involving separators , for eg :  '$OPERATOR( a + b ; c + d ; e + f )' is VALID 
    // use of blank-space-characters is mandatory before and after all independent-units such as operands, operators, separators, brackets, etc. 
        // {{ a } , { $SUM_(b) }} is NOT-VALID 
        // { { a } , { $SUM_( b ) } } is VALID 
        // (a+b) is NOT CORRECT 
        // ( a + b ) is CORRECT 
    
    
    
    VARIABLE NAME CONVENTIONS : 


    // must never contain '\b' ie backspace characters 

    // variable-name followed by '#' is used to add tags for matrix-index, matrix-size and derivative-order 
    // indices and size-indicators must be constant-valued numbers 
        // A#{3}{3} or A#{3;3} creates matrix-variable A of size 3x3 
    // variable size if not specified then is assumed to be matrix of size 1x1 
        // A is EQUIVALENT-TO A#{1;1} 
        // A#{1,1} is NOT-VALID 
        // A#{1;1} or A#{1}{1} is VALID 
    
    // "val(" is used to get value of an expression after '#' 
    // use of blank-spaces within "val( ... )" is mandatory 
    // use of blank-spaces outside "val( ... )" is not allowed 
        // A#_[val( 5 - 3 )][0]  is VALID 
        // A#_[_val( 5 - 3 )_][0]  is VALID 
        // A#_[_VAL( 5 - 3 )_][0]  is INVALID   ; since "VAL(" instead of "val(" 
        // A#_[_val_( 5 - 3 )_][0]  is INVALID   ; since "val_(" instead of "val(" 
        // A#_[_$val( 5 - 3 )_][0]  is INVALID   ; since "$val(" instead of "val(" 
        // A#_[_val( a + b )_][0]  is VALID   if 'a' and 'b' have values assigned to them  
    
    // IMPORTANT - Variable sizes cannot be changed later , so sizes must be specified during first use of variable 
    
    // A#[0][1] or A#[0,1] indicates element at index [0,1] of matrix A 
    // A#[][0] or A#[@,0] selects an array of elements from column-index 0 from all rows  ie { { A#[0][0] } , { A#[1][0] } , { A#[2][0] } } 
    // A#[0] or A#[0][@] or A#[0][] selects an array of all elements from row-index 0  ie { { A#[0][0] } , { A#[0][1] } , { A#[0][2] } } 
    
    
    // variable names must not begin with a Number or '+' or '-' 
        // 9AB is NOT-VALID variable-name 
        // -AB is NOT-VALID 
        // +AB is NOT-VALID 
        // -( AB ) is VALID 
    // IMPORTANT - variable-names must be preceeded and followed by blank-space 
    // variable-name must not contain any 'CH_terminator_for_variable_name'  ( ' ' ie blank-spaces ) except within "val( ... )" which is used to retrieve values of expressions for use within variable-specification 
    // variable-names must not contain any curly-braces  ie '(' or ')' especially mis-matching curly-braces 
        // '(' or ')' are allowed in variable tags 
        // A(2)#[1] is NOT-VALID 
    // if specification for index/array-size is not a number but an expression or variable then it must be enclosed in "val(" ... ")" directive  
        // eg - A#_[_val( 4 - ( 2 * 1 ) )_]_[0] is VALID and equivalent to A#[2][0] 
    // variable-name must not begin or end with 'ch_spacing_char_for_substring_after_tag_indicator' ( '_' ie underscore ) or any braces 
        // _AB is NOT-VALID variable-name 
    
    // d' or d is used to indicate derivate-order or derivative-wrt-variable 
        // A#d9 or A#d'9 or A#d(9) indicates 9'th derivative of A 
    // if derivative is with respect to a variable then it must be enclosed in braces 
        // A#d'x is NOT-VALID 
        // A#d'(x) is VALID 
        // A#d(x) or A#d'(x) or A#d(x,1) indicates first-derivative of A wrt x 
        // A#d(x,3) or A#d'(x,3) or A#d(x)_d3 or A#d(x)_d'3 indicates third-derivative of A wrt x 
    // Derivative-order must not be negative 
        // A#d(-2) is NOT VALID 
    // dp is used to indicate partial-derivative wrt some variable 
        // A#dp(y) indicates partial-derivative of A wrt y 
        // A#dp(y,3) indicates third-degree partial-derivative of A wrt y 
    // multiple derivative specifications are allowed 
        // A#dp(y,3)_d(x,2)_dp(z)_d'4  is equivalent to  "( 4'th derivative or ( partial-derivative wrt z of ( second-derivative wrt x of ( 3'rd degree partial-derivative wrt y of A ) ) ) )" 
    // order of derivative specification matters 
        // A#d(x)_d(y) is NOT-EQUIVALENT to A#d(y)_d(x) 
        // A#d'3_d(y) is NOT-EQUIVALENT to A#d(y)_d'3 
    // use of underscore between different specifications is mandatory 
        // A#d'3[0][0] is INVALID 
        // A#d'3_[0][0] is VALID 
        
        
        
        

    FUNDAMENTAL OPERATORS : 
    
    
    1. Assignment Operator 
        Notation : [ ":=" ] 
    Registers Values to variables and expression-identifiers.
        eg -  A := ( 5 + 2 ) 
        eg -  \expr1 := ( A + B ) 
    
    
    2. Null Assignment 
        Notations : 
            [ "$NULL_(" , "$NULL(" , "$null_(" , "$null(" ] 
    Clears previously assigned value to a variable-identifier or expression-identifier. 
        eg -  A := $NULL_( 0 ) 
        eg -  A#[][0] := $NULL_( 0 ) 
        eg -  \expr1 := $NULL_( 0 ) 
    
    
    3. Equals-To 
        Notations : 
            [ "==" , "=" ] 
    Used to indicate Equality in expressions.
        eg -  A == B + 5 
            Creates the expression "( A == B + 5 )" 
            This does not assign any values.
    
    
    4. Apply Operator 
        Notations : 
            [ "$APPLY_(" , "$APPLY(" , "$apply_(" , "$apply(" , 
              "$APP_(" , "$APP(" , "$app_(" , "$app(" ] 
    Expects only 1 Operand and applies the Operator if present in the first-operand. 
    Does not apply all operators present in sub-expression, merely the first-operator in its operand.
        eg -  $APP_( $SOLVE_( ... ) ) 
            This gives solution to Linear System whereas "$SOLVE_( ... )" simply creates an expression with Solve-Linear-System Operator which does not actually solve its contents.
    If Operand is a matrix then application is performed to first-operator of all elements of matrix 
    
    
    5. Apply-All Operator 
        Notations : 
            [ "$APPLY_ALL_(" , "$APPLY_ALL(" , "$apply_all_(" , "$apply_all(" , 
              "$APP_ALL_(" , "$APP_ALL(" , "$app_all_(" , "$app_all(" ] 
    Expects only 1 Operand.
    Applies all Operators of all sub-expressions of operand, starting from inner-most sub-expressions then working its way to operators in outer-expressions.
        eg -  $APP_ALL_( { 1 , 2 , 1 } + $SOLVE_( ... ) ) 
    
    
    
    OPERATORS That Require To Be APPLIED : 
    
        If operators are not applied then they remain in expression as operators without their functionality-performed on the operands. 
            eg -  If  { a := 3  ;  b := -1 } 
            then  " \expr1 := $APP_( $VAL_( a + b ) ) "  yields  " 2 "  being assigned to Expression-Identifier '\expr1' 
            however  " \expr1 := $VAL_( a + b ) "  yields  " $VAL_( a + b ) "  being assigned to Expression-Identifier '\expr1' 
    
    
    1. Numeric Value Calculation 
        Notations : 
            [ "$VAL_(" , "$VAL(" , "$val_(" , "$val(" ] 
    When applied, it yields the Numeric Value of an expression by substituting variables by their assigned values 
        Gives Error if any of the used variables do not have a value assigned 
        eg -  If  { a := 3  ;  b := -1 } 
            then  " c := $APP_( $VAL_( a + b ) ) "  yields  " c := 2 " 
    
    
    2. Matrix To Scalar 
        Notations : 
            [ "$[MATRIX_TO_SCALAR]_(" , 
              "$[MAT_TO_SCALAR]_(" , 
              "$MATRIX_TO_SCALAR_(" , "$MATRIX_TO_SCALAR(" , "$matrix_to_scalar_(" , "$matrix_to_scalar(" , 
              "$MAT_TO_SCALAR_(" , "$MAT_TO_SCALAR(" , "$mat_to_scalar_(" , "$mat_to_scalar(" , 
              "$MAT_TO_SCLR_(" , "$MAT_TO_SCLR(" , "$mat_to_sclr_(" , "$mat_to_sclr(" ] 
    When applied, it converts a single expression with matrix-Variables and matrix-operators to equivalent Matrix-of-Expressions with scalar-Variable-equivalents of matrix-Variables and without matrix-operators.
        eg -  $APP_( $MATRIX_TO_SCALAR_( A#{2;2} ) )
            above yields -  { { A#[0][0] , A#[0][1] } , { A#[1][0] , A#[1][1] } } 
        eg -  $APP_( $MATRIX_TO_SCALAR_( A#{1;2} [+] B#{1}{2} ) )
            above yields -  { {  ( A#[0][0] + B#[0][0] )  ;  ( A#[0][1] + B#[0][1] ) } } 
            ie element-wise addition of matrix-elements 
    
    
    3. Size Of Operator 
        Notations : 
            [ "$SIZE_(" , "$SIZE(" , "$size_(" , "$size(" , 
              "$SIZE_OF_(" , "$SIZE_OF(" , "$size_of_(" , "$size_of(" ] 
    When applied, it yields size of a matrix-of-expressions.
    First Element returned indicates number of Rows.
    If all rows are of same size, then Second Element returned indicates number of Columns. ie size-of-each row.
    If all rows are NOT of same size, then Second Element is an array of Integers specifying row-size of each row. 
        eg -  $APP_( $SIZE_( A#{3;5} ) )  yields  { { 3 } , { 5 } } 
        eg -  If  " \expr1  :=   { { 1 } , { 1 , 2 , 3 } } " 
        eg -  $APP_( $SIZE_( \expr1 ) )  yields  { { 2 } , { 1 , 3 } } 
    
    
    4. Solve System Of Linear Equations 
        Notations : 
            [ "$SOLVE_(" , "$SOLVE(" , "$solve_(" , "$solve(" ] 
    Solves a system of Linear-Equations 
        Can solve an Overdetermined but Consistent System, in which case some of the extra variables take value Zero in result. 
    When applied, it yields an array of numeric-values whose ordering correspond to ordering of Solving-Variables specified.
    Alternative 1 - 
        First-Operand is expected to be an array of equations.
            If lacks equals-to sign then expression is assumed to be equal-to zero.
        Second-Operand is expected to be an array of Solving-Variables.
            Any variables which are not Solving-Variables are replaced by values assigned to them. 
            Generates Error if a variable is present with no assigned value. 
        Generates Error if equations are not Linear wrt Solving Variables.
    Alternative 2 - 
        Format is of type  "( [A] * [X]  ==  [B] )"  where  [A] is First-Operand  ,  [B] is Second-Operand  ,  [X] indicates solving-variables which are not to be specified. 
        First-Operand is a matrix-of coefficients-of-solving-variables per equation. 
            A[ equation-index ][ solving-variable-index ] 
        Second-Operand is an array of numeric-values corresponding to each equation.
            B[ equation-index ]
            Notice that these values lie on the other-side of the Equality. 
    
    
    5. Symbolic Solve a Linear-System 
        Notations :
            [ "$SYM_SOLVE_(" , "$SYM_SOLVE(" , "$sym_solve_(" , "$sym_solve(" ] 
    When applied, it yields an array of equations of type "( Solving-Variable == solution-Expression )" 
    Ordering of solution DOES NOT Correspond to ordering of Solving-Variables. 
    Generates Error if equations are not Linear wrt Solving Variables.
        First-Operand is expected to be an array of equations.
            If lacks equals-to sign then expression is assumed to be equal-to zero.
        Second-Operand is expected to be an array of Solving-Variables.
    
    
    6. Solve Quadratic 
        Notations : 
            [ "$QUADRATIC_(Ax2,Bx,C=0)_(" , 
              "$SOLVE_QUADRATIC_(" , "$SOLVE_QUADRATIC(" , "$solve_quadratic_(" , "$solve_quadratic(" , 
              "$SOLVE_QUAD_(" , "$SOLVE_QUAD(" , "$solve_quad_(" , "$solve_quad(" , 
              "$QUADRATIC_SOLVE_(" , "$QUADRATIC_SOLVE(" , "$quadratic_solve_(" , "$quadratic_solve(" , 
              "$QUAD_SOLVE_(" , "$QUAD_SOLVE(" , "$quad_solve_(" , "$quad_solve(" , 
              "$QUADRATIC_(" , "$QUADRATIC(" , "$quadratic_(" , "$quadratic(" , 
              "$QUAD_(" , "$QUAD(" , "$quad_(" , "$quad(" ] 
    Three Operands Expected corresponding to A, B, C such that  "( ( A * x^2 ) + ( B * x ) + C  ==  0 )"
    
    
    7. Replacement of Variable 
        Notations : 
            [ "$REPLACE_(EXPR,VAR,REPLACEMENT)_(" , 
              "$REPLACE_(" , "$REPLACE(" , "$replace_(" , "$replace(" , 
              "$RPLC_(" , "$RPLC(" , "$rplc_(" , "$rplc(" ] 
    When applied, replaces a Variable by an Expression in the First-Operand. 
    Second-Operand is  a variable to be replaced-by  or  equation with Variable on LHS and replacement-expression on RHS. 
    Third-Operand is replacement-expression if second-operand is not an equation. 
        All operands are allowed to be arrays or matrices if multiple replacements are to be performed. 
        In case of multiple replacement-variables, different ordering of variables-to-be-replaced can give varied results. 
    Operands are stored at 'this.expr_operands_array[]' in both cases ie 2 or 3 operands, and both operands are column-matrices 
    
    
    8. Get N'th Operand ( N >= 1 )
        Notations : 
            [ "$GET_OPERAND_(" , "$GET_OPERAND(" , "$get_operand_(" , "$get_operand(" ) 
              "$OPERAND_N_(" , "$OPERAND_N(" , "$operand_n_(" , "$operand_n(" ] 
    When applied, retrieves the N'th Operand of operator that is directly present in its First-Operand.
    First-Operand is the expression from which N'th operand is to be retireved-From. 
    Second-Operand is a positive integer 'N' indicating the ordinality of operand-to-be-retrieved from the expression in First-Operand.
        eg -  $APP_( $OPERAND_N_( ( A + ( B + C ) ) , 2 ) ) 
            yields  ( B + C ) 
    
    
    9. Concatenate 
        Notations : 
            [ "$CONCAT_(" , "$CONCAT(" , "$concat_(" , "$concat(" , 
              "$CONCATENATE_(" , "$CONCATENATE(" , "$concatenate_(" , "$concatenate(" ] 
    Any number of operands may be present.
    Operands may or may not be matrices of any size.
    When applied, it yields an array with all operands concatenated into array in order.
    Matrices if present are converted to an array with matrix-rows concatenated in order.
    
    
    10. Derivative 
        Notations : 
            [ "$D/Dt_(" , "$d/dt_(" ] 
    When applied, it yields derivative of an expression.
    Derivative of variable A would be A#_d'1 
    
    
    11. Derivative degree-N 
        Notations : 
            [ "$D^n/Dt^n_(" , "$d^n/dt^n_(" , "$D^n/Dt^n_(Fx,x)_(" , "$d^n/dt^n_(Fx,x)_(" ] 
    When applied, it yields N'th derivative of First-Operand.
    Second-Operand is a positive integer indicating derivative-order.
    
    
    12. Derivative wrt Variable 
        Notations : 
            [ "$DERIV_(" , "$DERIV(" , "$deriv_(" , "$deriv(" , 
              "$DERIVATIVE_(" , "$DERIVATIVE(" , "$derivative_(" , "$derivative(" , 
              "$D(Fx)/Dx_(" , "$d(Fx)/dx_(" , "$d(fx)/dx_(" , "$d(f(x))/dx_(" , 
              "$D(Fx)/Dx_(Fx,x)_(" , "$d(Fx)/dx_(Fx,x)_(" ] 
    When applied, it yields derivative of First-Operand wrt derivative-variable specified by Second-Operand.
    
    
    13. N'th Derivative wrt Variable 
        Notations : 
            [ "$DERIV_N_(" , "$DERIV_N(" , "$deriv_n_(" , "$deriv_n(" , 
              "$DERIVATIVE_N_(" , "$DERIVATIVE_N(" , "$derivative_n_(" , "$derivative_n(" , 
              "$D^n(Fx)/Dx^n_(" , "$D^n(Fx)/Dx^n_(Fx,n,x)_(" ] 
    When applied, it yields derivative of First-Operand wrt derivative-variable specified by Third-Operand, degree of derivative is indicated by Second-Operand. 
        eg -  $DERIV_N_( ( x ^ 3 ) , 2 , x )  yields second-derivative of  ( x ^ 3 ) wrt x 
    
    
    14. Partial Derivative wrt Variable 
        Notations : 
            [ "$PART_DERIV_(Fx,x)_(" , 
              "$PART_DERIV_(" , "$PART_DERIV(" , "$part_deriv_(" , "$part_deriv(" , 
              "$PARTIAL_DERIVATIVE_(" , "$PARTIAL_DERIVATIVE(" , "$partial_derivative_(" , "$partial_derivative(" , 
              "$dp(Fx)/dp(x)_(" , "$dp(fx)/dp(x)_(" , "$dp(f(x))/dp(x)_(" , 
              "$dp(Fx)/dp(x)_(Fx,x)_(" ] 
    When applied, yields partial-derivative of First-Operand wrt derivative-variable specified by Second-Operand.
    Partial-Derivative of any variable wrt any other-variable is taken to be zero. 
        Thus resultant expression will contain only partial-derivative variable.
    
    
    15. N'th Derivative wrt Variable 
        Notations : 
            [ "$PART_DERIV_N_(Fx,n,x)_(" ) , 
              "$PART_DERIV_N_(" , "$PART_DERIV_N(" , "$part_deriv_n_(" , "$part_deriv_n(" , 
              "$PARTIAL_DERIVATIVE_N_(" , "$PARTIAL_DERIVATIVE_N(" , "$partial_derivative_n_(" , "$partial_derivative_n(" , 
              "$dp^n(Fx)/dp(x)^n_(" , "$dp^n(fx)/dp(x)^n_(" , "$dp^n(f(x))/dp(x)^n_(" , 
              "$dp^n(Fx)/dp(x)^n_(Fx,n,x)_(" ] 
    When applied, it yields derivative of First-Operand wrt derivative-variable specified by Third-Operand, degree of derivative is indicated by Second-Operand. 
        eg -  $PART_DERIV_N_( ( x ^ 3 ) , 2 , x )  yields second-degree partial-derivative of  ( x ^ 3 ) wrt x 
    Partial-Derivative of any variable wrt any other-variable is taken to be zero. 
        Thus resultant expression will contain only partial-derivative variable.
    
    
    
    
    MATRIX OPERATORS : 
        ( Can Be Applied to yield matrix-of-scalar expressions ) 
        ( Matrices with only one column and multiple rows are taken to be VECTORS ) 
            eg - { a , b , c } is VECTOR 
            eg - { { a , b , c } } is NOT VECTOR 
            eg - { { a } , { b } , { c } } is VECTOR 
    
    
    1. Cross Product Of Vectors 
        Notations : 
            [ "[x]" , 
              "CROSS_PRODUCT_(" , "CROSS_PRODUCT(" , "cross_product_(" , "cross_product(" ] 
    
    
    2. Dot Product Of Vectors 
        Notations : 
            [ case( "[.]" ) : 
              "DOT_PRODUCT_(" , "DOT_PRODUCT(" , "dot_product_(" , "dot_product(" ) ]
    
    
    3. Vector of Zeros 
        Notation : 
            [ "$[V0]_(" , 
              "$V0_(" , "$V0(" , "$v0_(" , "$v0(" ] 
    Represents, a vector of as many Ones as specified by First-Operand. 
    
    
    4. Vector of Ones 
        Notation : 
            [ "$[V1]_(" , 
              "$V1_(" , "$V1(" , "$v1_(" , "$v1(" ]
    Represents, a vector of as many Ones as specified by First-Operand. 
    
    
    5. Matrix of Specified Element (4 Operands) 
        Notations : 
            [ "$[V(n)]_(" ) , 
              "$[V(n)]_(default,len,elm,pos)_(" ) , 
              "$[MAT(N_x_M)]_(" , 
              "$[MAT(N_x_M)]_(default,size,elm,pos)_(" , 
              "$MAT_N_x_M_(" ) , "$MAT_N_x_M(" , "$mat_n_x_m_(" , "$mat_n_x_m(" , 
              "$MATRIX_N_x_M_(" , "$MATRIX_N_x_M(" , "$matrix_n_x_m_(" , "$matrix_n_x_m(" ] 
    First-Operand indicates the Default element of the vector/matrix. 
    Second-operand must be a vector of length '1' or '2' indicating size of resultant vector/matrix. 
    Third-operand indicates expression to place at the position-indices indicated by the Fourth-operand.
    Fourth-operand is an array of indices at which expression in second-operand is to be place. 
        eg -  $APP_( $[MAT(N_x_M)]_(default,size,elm,pos)_( 0.0 , { 2 , 2 } , 1.0 , { { 0 , 1 } , { 1 , 0 } } ) ) 
            yields { { 0.0 , 1.0 } , { 1.0 , 0.0 } } 
    
    
    6. Replace Matrix Elements
        Notations : 
            [ "$[REPLACE]_(MAT,VAL,INDX)_(" , 
              "$[RPLC]_(" , 
              "$RPLC_INDEX_(" , "$RPLC_INDEX(" , "$rplc_index_(" , "$rplc_index(" , 
              "$REPLACE_INDEX_(" , "$REPLACE_INDEX(" , "$replace_index_(" , "$replace_index(" ]
    First-Operand is Matrix in which replacement is to be done. 
    Second-Operand is a Vector of Replacement Elements, which are in one-to-one correspondence with replacement-indices.
    Third-Operand is a vector of matrix-indices which are to be replaced in first-operand by elements in second-operand. eg -  { { row-index-1 , col-index-1 } , { row-index-2 , col-index-2 } , ... } 
        Row or Col index '-1' is used to denote All-Rows or All-Columns. 
    
    
    7. Select From Matrix 
        Notations : 
            [ "$[SEL]_(" , 
              "$[SEL]_([MAT_FROM],[MAT_OF_SEL_INDICES])_(" , 
              "$SEL_(" , "$SEL(" , "$sel_(" , "$sel(" , 
              "$SELECT_MATRIX_POSITION_(" , "$SELECT_MATRIX_POSITION(" , "$select_matrix_position_(" , "$select_matrix_position(" ]
    First-Operand is matrix from which elements are to be selected. 
    Second-Operand is a matrix which may contain expressions or indices-to-first-operand.
        Row or Col index '-1' is used to denote All-Rows or All-Columns. 
        Important to Note that matrix of indices is a 3D-array, so it is important to enclose the pair-of-indices within round brackets, otherwise an exception is raised since 3D-arrays are not allowed. 
            eg -  { { { 1 , 1 } } , { { 1 , 0 } } }  is INVALID as it is a 3D-array 
            eg -  { { ( { 1 , 1 } ) } , { ( { 1 , 0 } ) } }  is VALID since it is an array within 2D-array ( which is indirectly a 3D-array though not directly )
    eg -  $APP_( $[SEL]_(  { { a , b } , { c , d } }  ,  { { ( { -1 , 0 } ) } , { F , G } , { ( { 1 , 1 } ) , ( { 1 , 0 } ) } }  ) ) 
        above yields  { { a , c } , { F , G } , { d , c } } 
    
    
    8. Transpose Of Matrix 
        Notations : 
            [ "^[T]" ) , 
              "$[TRANSPOSE]_(" ) , 
              "$MAT_TRNSP_(" , "$MAT_TRNSP(" , "$mat_trnsp_(" , "$mat_trnsp(" , 
              "$TRANSPOSE_(" , "$TRANSPOSE(" , "$transpose_(" , "$transpose(" , 
              "$TRNSP_(" , "$TRNSP(" , "$trnsp_(" , "$trnsp(" ] 
    Represents Transpose of the Matrix in First-Operand. 
    
    
    9. Determinant 
        Notations : 
            [ "$[DET]_(" , 
              "$DET_(" , "$DET(" , "$det_(" , "$det(" , 
              "$DETERMINANT_(" , "$DETERMINANT(" , "$determinant_(" , "$determinant(" ] 
    Represents the determinant of the matrix in First-Operand. 
    
    
    10. Matrix Inverse 
        Notations : 
            [ "$[INV]_(" , 
              "^[-1]" ) , 
              "MAT_INV_(" , "MAT_INV(" , "mat_inv_(" , "mat_inv(" , 
              "MATRIX_INVERSE_(" , "MATRIX_INVERSE(" , "matrix_inverse_(" , "matrix_inverse(" ] 
    Represents the Inverse of Matrix in First-Operand. 
    
    
    11. Matrix-Of-Minors of a Matrix
        Notations : 
            [ "$[MINORS]_(" , 
              "$MAT_OF_MINORS_(" , "$MAT_OF_MINORS(" , "$mat_of_minors_(" , "$mat_of_minors(" , 
              "$MATRIX_OF_MINORS_OF_(" , "$MATRIX_OF_MINORS_OF(" , "$matrix_of_minors_of_(" , "$matrix_of_minors_of(" ] 
    Represents the Matrix-Of-Minors of the Matrix in First-Operand.
    Each element of resultant-matrix is the 'Minor' of that position in the original Matrix.
    
    
    12. Minor of element at specified-position in Matrix 
        Notations : 
            [ "$[MINOR_OF_POS]_(MAT,POS)_(" ) , 
              "$[MINOR_OF_POS]_(" , 
              "$MINOR_OF_POS_(" , "$MINOR_OF_POS(" , "$minor_of_pos_(" , "$minor_of_pos(" , 
              "$MINOR_OF_POSITION_(" , "$MINOR_OF_POSITION(" , "$minor_of_position_(" , "$minor_of_position(" ] 
    Represents the 'Minor' of element at specified-position in the Matrix given by First-Operand. 
    
    
    13. Apply Sign Of Cofactors to a Matrix 
        Notations : 
            [ "$[APPLY_SIGN_OF_COFACTORS]_(" , 
              "$[SIGN_OF_COF]_(" , 
              "$APPLY_SIGN_OF_COFACTORS_(" , "$APPLY_SIGN_OF_COFACTORS(" , "$apply_sign_of_cofactors_(" , "$apply_sign_of_cofactors(" , 
              "$SIGN_OF_COF_(" , "$SIGN_OF_COF(" , "$sign_of_cof_(" , "$sign_of_cof(" , 
              "$[+,-]_(" , 
              "$[+1,-1]_(" ]
    When applied, applies the sign of Cofactors of a Matrix which is alternate application of '+1' and '-1' to elements of even rows of matrix, and alternate application of '-1' and '+1' to odd rows of matrix.
    When applied to matrix of Minors, gives matrix of Cofactors. 
    
    
    14. Element-Wise Custom-Operator for two Matrices 
        Notations : 
            [ "$[ELEMENT_WISE_OPERATOR]_([M1],[M2],$OPR)_(" , 
              "$[ELEMENT_WISE_OPERATOR]_(" , 
              "$([M1],[M2],$OPR)_(" , 
              "$ELEMENT_WISE_OPERATOR_(" , "$ELEMENT_WISE_OPERATOR(" , "$element_wise_operator_(" , "$element_wise_operator(" ] 
    Represents a matrix with specified-operator applied to corresponding elements of the two operand-matrices.
    First-Operand is a Matrix.
    Second-Operand is also a Matrix.
    Third-Operand must be any algebraic expression with a binary-operator, considered as the operator to be applied to the corresponding elements of first two operands. 
    Resultant is a matrix consisting of elements of binary-operator applied to corresponding-elements of the two matrices. 
        Size of resultant matrix is increased in case of size mismatch and one-of-the two-operands within third-operand is taken as the corresponding default element. 
    eg -  $APP_( $([M1],[M2],$OPR)_(  { { a , b } }  ,  { { c } , { d } }  ,  ( 0 / 1 )  ) ) 
        above yields  {  {  ( a / c )  ,  ( b / 1 )  }  ,  {  ( 0 / c )  ,  ( 0 / d )  }  } 
    
    
    15. Add All Elements Of Matrix 
        Notations : 
            [ "$[ADD_ALL]_(" , 
              "$ADD_ALL_(" , "$ADD_ALL(" , "$add_all_(" , "$add_all(" , 
              "$ADD_ELEMENTS_OF_MATRIX_(" , "$ADD_ELEMENTS_OF_MATRIX(" , "$add_elements_of_matrix_(" , "$add_elements_of_matrix(" ] 
    Represents addition of all elements of Matrix in First-Operand. 
    
    
    16. Multiply All Elements Of Matrix 
        Notations : 
            [ "$[MUL_ALL]_(" , 
              "$MUL_ALL_(" , "$MUL_ALL(" , "$mul_all_(" , "$mul_all(" , 
              "$MULTIPLY_ELEMENTS_OF_MATRIX_(" , "$MULTIPLY_ELEMENTS_OF_MATRIX(" , "$multiply_elements_of_matrix_(" , "$multiply_elements_of_matrix(" ] 
    Represents addition of all elements of Matrix in First-Operand. 
    
    
    17. Identity Matrix 
        Notations : 
            [ "$[I]_(" , "$[I](" ] 
    Represents a Square-Matrix which is a Unit-Matrix or Identity-Matrix of specified size. 
    First-Operand is expected to be an integer indicating size of resultant Square-Matrix.
    
    
    18. Matrix of Ones or Zeros 
        Notations : 
            [ "$[1]_(" , "$[1](" ] 
            [ "$[0]_(" , "$[0](" ] 
    First-Operand indicates number-of-rows.
    Second-Operand indicates number-of-cols.
    
    
    19. Eigen Values Array (gives Real Counterpart Only) 
        Notations : 
            [ "$[EigValArr_Real]_(" , 
              "$EIGEN_VALUES_REAL_(" , "$EIGEN_VALUES_REAL(" , "$eigen_values_real_(" , "$eigen_values_real(" ] 
    First-Operand is a Matrix.
    Can only be applied to Constant-Valued Matrix and not a Matrix-Of-Expressions.
    
    
    20. Eigen Values Array (gives Imaginary Counterpart Only) 
        Notations : 
            [ "$[EigValArr_Imaginary]_(" , 
              "$EIGEN_VALUES_IMAGINARY_(" , "$EIGEN_VALUES_IMAGINARY(" , "$eigen_values_imaginary_(" , "$eigen_values_imaginary(" ] 
    First-Operand is a Matrix.
    Can only be applied to Constant-Valued Matrix and not a Matrix-Of-Expressions.
    
    
    21. Eigen Vector Matrix 
        Notations : 
            [ "$[EigVect]_(" , 
              "$EIGEN_VECTORS_(" , "$EIGEN_VECTORS(" , "$eigen_vectors_(" , "$eigen_vectors(" ] 
    First-Operand is a Matrix.
    Can only be applied to Constant-Valued Matrix and not a Matrix-Of-Expressions.
    Gives an array of Eigen-Vectors.
    Each Row in resultant represents an Eigen-Vector.
    
    
    22. Cross-Product Matrix of a Vector 
        Notations : 
            [ "$[CROSS_PRODUCT_MATRIX_OF_VECTOR]_(" , 
              "$CROSS_PRODUCT_MATRIX_(" , "$CROSS_PRODUCT_MATRIX(" , "$cross_product_matrix_(" , "$cross_product_matrix(" , 
              "$[MAT[x]_(V)]_(" ] 
    Represents Cross-Product Matrix of a Vector of length 3.
    
    
    23. Diagonal Matrix of a Vector 
        Notations : 
            [ "$[DIAG]_(" , 
              "$[DIAG]_(V)_(" , 
              "$DIAGONAL_MATRIX_OF_VECTOR_(" , "$DIAGONAL_MATRIX_OF_VECTOR(" , "$diagonal_matrix_of_vector_(" , "$diagonal_matrix_of_vector(" , 
              "$[Diagonal_Matrix_(Vect)]_(" ] 
    Returns diagonal-matrix of an array with non-diagonal elements as zero.
    First-Operand is an Array.
    
    
    
    
    BASIC MATRIX OPERATORS 
    
    Matrix-Addition :  [+]              Addition :  +               Division :  / 
    Matrix-Subraction :  [-]            Subraction :  - 
    Matrix-Multiplication :  [*]        Multiplication :  * 
    Matrix-Power :  [^]                 Raised-To Power :  ^ 
    
    
    
    
    GENERAL OPERATORS 
    
    1. Summation 
        Notations : 
            [ "$SUM_(" , "$SUM(" , "$sum_(" , "$sum(" ]
    eg -  $SUM_( a + b + c + d ) 
    eg -  $SUM_( a , b , c , d ) 
    
    
    2. Product of Multiple Elements
        Notations : 
            [ "$PRD_(" , "$PRD(" , "$prd_(" , "$prd(" ] 
    eg -  $PRD_( a * b * c * d ) 
    eg -  $PRD_( a , b , c , d ) 
    
    
    3. Negative Of 
        Notation :  [ "-(" ]
    
    
    4. Power 
        Notation :  [ "^" ] 
    
    
    5. Root N 
        Notations :  
            [ "$ROOT_N_(x,N)_(" , 
              "$ROOT_N_(" , "$ROOT_N(" , "$root_n_(" , "$root_n(" , 
              "^(1/)" ] 
    
    
    6. Absolute Unsigned 
        Notations : 
            [ "$ABS_(" , "$ABS(" , "$abs_(" , "$abs(" , 
              "$ABSOLUTE_(" , "$ABSOLUTE(" , "$absolute_(" , "$absolute(" , 
              "$UNSIGNED_(" , "$UNSIGNED(" , "$unsigned_(" , "$unsigned(" ] 
    
    
    7. Inversion 
        Notations : 
            [ "^(-1)" , 
              "$(1)/(" , 
              "$1/(" , 
              "$INV_(" , "$INV(" , "$inv_(" , "$inv(" , 
              "$INVERSION_(" , "$INVERSION(" , "$inversion_(" , "$inversion(" ] 
    
    
    8. Square 
        Notations : 
            [ "$SQUARE_(" , "$SQUARE(" , "$square_(" , "$square(" , 
              "$SQR_(" , "$SQR(" , "$sqr_(" , "$sqr(" , 
              "^(2)" ] 
    
    
    9. Square Root 
        Notations : 
            [ "$SQUARE_ROOT_(" , "$SQUARE_ROOT(" , "$square_root_(" , "$square_root(" , 
              "$SQRT_(" , "$SQRT(" , "$sqrt_(" , "$sqrt(" , 
              "^(1/2)" ] 
    
    
    10. Exponent 
        Notations : 
            [ "$EXP_(" , "$EXP(" , "$exp_(" , "$exp(" , 
              "$(e)^(" ] 
    
    
    11. Logarithm Natural 
        Notations : 
            [ "$LOG_E_(" , "$LOG_E(" , "$log_e_(" , "$log_e(" , 
              "$LOG_(" , "$LOG(" , "$log_(" , "$log(" , 
              "$LOG_NATURAL_(" , "$LOG_NATURAL(" , "$log_natural_(" , "$log_natural(" ] 
    
    
    12. Logarithm to Base 
        Notations : 
            [ "$LOG_(x,B)_(" , 
              "$LOG_B_(" , "$LOG_B(" , "$log_b_(" , "$log_b(" , 
              "$LOG_BASE_(" , "$LOG_BASE(" , "$log_base_(" , "$log_base(" ] 
    
    
    13. Modulus / Remainder 
        Notations : 
            [ "$MODULUS_(" , "$MODULUS(" , "$modulus_(" , "$modulus(" , 
              "$MOD_(" , "$MOD(" , "$mod_(" , "$mod(" , 
              "%M%" 
              "%" , 
              "$REMAINDER_(" , "$REMAINDER(" , "$remainder_(" , "$remainder(" , 
              "%R%" ] 
    
    
    14. Quotient 
        Notations : 
            [ "$QUOTIENT_(" , "$QUOTIENT(" , "$quotient_(" , "$quotient(" , 
              "$QUO_(" , "$QUO(" , "$quo_(" , "$quo(" , 
              "%Q%" ] 
    
    
    15. Floor Integer 
        Notations : 
            [ "$INT_FLOOR_(" , "$INT_FLOOR(" , "$int_floor_(" , "$int_floor(" , 
              "$FLOOR_(" , "$FLOOR(" , "$floor_(" , "$floor(" ]
    
    
    16. Ceiling Integer 
        Notations : 
            [ "$INT_CEIL_(" , "$INT_CEIL(" , "$int_ceil_(" , "$int_ceil(" , 
              "$CEIL_(" , "$CEIL(" , "$ceil_(" , "$ceil(" ] 
    
    
    17. Sine 
        Notations : 
            [ "$SIN_(" , "$SIN(" , "$sin_(" , "$sin(" ] 
    
    
    18. Cosine 
        Notations : 
            [ "$COS_(" , "$COS(" , "$cos_(" , "$cos(" ] 
    
    
    19. Sine-Inverse 
        Notations : 
            [ "$SIN^(-1)_(" , "$sin^(-1)_(" , 
              "$A_SIN_(" , "$A_SIN(" , "$a_sin_(" , "$a_sin(" , 
              "$ASIN_(" , "$ASIN(" , "$asin_(" , "$asin(" , 
              "$SIN_INV_(" , "$SIN_INV(" , "$sin_inv_(" , "$sin_inv(" ] 
    
    
    20. Cosine-Inverse 
        Notations : 
            [ "$COS^(-1)_(" , "$cos^(-1)_(" , 
              "$A_COS_(" , "$A_COS(" , "$a_cos_(" , "$a_cos(" , 
              "$ACOS_(" , "$ACOS(" , "$acos_(" , "$acos(" , 
              "$COS_INV_(" , "$COS_INV(" , "$cos_inv_(" , "$cos_inv(" ] 
        
    
    
    // EXPRESSION-IDENTIFIERS : 
    
    // Expression-Identifiers are NOT Variables but are similar Variables 
    // Expression-Identifiers begin with '\' 
        // \A is an Expression-Identifier and not a variable 
        // A is a variable 
    
    // variables can only be assigned constant-values and cannot not be assigned expressions 
        // A := ( B + C ) is NOT-VALID 
        // \A := ( B + C ) is VALID  sinc \A is Expression-Identifier 
        // \A := 9 is also VALID 
        // A := ( 9 + $VAL_( B + C ) ) is VALID if B and C have values assigned 
    
    // Expression-Identifiers on RHS of assignment are replaced by the expression they hold 
        // executing  \A := { { B , E } , { C + D ; E + F } }  assigns the given matrix-of-expressions to Expression-Identifier \A 
        // then executing  ( A * \A#[0][1] )  gives  ( A * ( C + D ) )  since \A#[0][1] is replaced by the expression ( C + D ) 
