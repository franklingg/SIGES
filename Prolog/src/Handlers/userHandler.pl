/** <module> UserHandler
* Módulo contendo as operações básicas de criação, leitura e deleção de usuários no sistema SIGES.
*/
:- module(userHandler, [createUser/4, checkValidEmail/1, checkValidPassword/1, 
                        checkValidName/1, login/2]).

:- encoding(utf8).

:- use_module(library(crypto)).

:- use_module('./../utils.pl').
:- use_module('./dataHandler.pl').

/**
 * createUser(-Name:string, -Email:string, -Password:string, -IsAdm:boolean) is semidet. 
 * 
 * Registra um novo usuário no sistema, dadas as suas informações, falha caso o usuário já
 * esteja cadastrado.
 * @param Name Nome do usuário a ser cadastrado
 * @param Email Email do usuário a ser cadastrado
 * @param Password Senha do usuário a ser cadastrado
 * @param IsAdm Valor que define se o usuário é um adminstrador ou usuário comum
 */
createUser(Name, Email, Password, IsAdm):-
    utils:timeNow(D),
    (\+ dataHandler:existsUserFile,!;dataHandler:notExistingUser(Email)),
    crypto_password_hash(Password, PasswordHash),
    U= userFull(Name, Email, PasswordHash, IsAdm, D),
    dataHandler:saveUser(U),
    login(Email, Password),!;
    errorHandler:promptError(7),fail.

/**
 * checkValidEmail(-Email:string) is semidet. 
 * 
 * Checa se um Email é válido, isto é, tem o formato XX@XX.XX
 * @param Email Email a ser checado
*/
checkValidEmail(Email):-
    wildcard_match("[a-z]*@[a-z]*.[a-z]*", Email, [case_sensitive(false)]),!;
    errorHandler:promptError(2),fail.

/**
 * checkValidPassword(-Password:string) is semidet. 
 * 
 * Checa se uma senha é válida, isto é, se possui pelo menos 8 caracteres.
 * @param Password Senha a ser checada
*/
checkValidPassword(Password):-
    string_length(Password, Length), Length >= 8, !;
    errorHandler:promptError(3),fail.

/**
 * checkValidName(-Name:string) is semidet. 
 * 
 * Checa se um nome é válido, isto é, se possui pelo menos 2 caracteres.
 * @param Name Nome a ser checada
*/
checkValidName(Name):-
    string_length(Name, Length), Length >= 2, !;
    errorHandler:promptError(6),fail.

/**
 * login(-Email:string, -Password:string) is semidet. 
 * 
 * Recebe um Email e Senha e, em caso de validade, registra o usuário como logado.
 * @param Email Email do usuário a ser logado
 * @param Password Senha do usuário a ser logado
 */
login(Email,Password):-
    dataHandler:readUsers,
    bagof(_,dataHandler:userFull(Name,Email,Hash,IsAdm,_),List),
    dataHandler:cleanUsers,
    atom_string(PasswordHash, Hash),
    List\=[],crypto_password_hash(Password,PasswordHash), 
    assertz(dataHandler:user(Name,Email,IsAdm)),!;
    errorHandler:promptError(4),fail.

/**
 * deleteUser(-Email:string) is semidet. 
 * 
 * Recebe um Email, em caso de validade, deleta o usuário do sistema.
 * @param Email Email do usuário a ser deletado
 */
deleteUser(Email):-
    dataHandler:readUsers, 
    findall(_,dataHandler:userFull(_,Email,_,_,_),List),List\=[],
    retract(dataHandler:userFull(_,Email,_,_,_)),
    dataHandler:writeUsers,!;
    errorHandler:promptError(8),fail.