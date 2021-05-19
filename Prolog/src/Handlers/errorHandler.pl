/** <module> ErrorHandler
*  Módulo contendo operações de lidar com erros no sistema SIGES
*/
:- module(errorHandler, [promptError/1]).

:- encoding(utf8).

/** 
* promptError(-N: int) is semidet.
*
* Imprime a mensagem de erro adequada dado o código de erro fornecido (falha caso não exista
* mensagem para o código fornecido).
* @param N Código de erro a ser impresso
*/
promptError(0).

promptError(1):- ansi_format([bold,fg(red)], "~w", ["Opção inválida. Tente novamente."]), nl.

promptError(2):- ansi_format([bold,fg(red)], "~w", ["Email inválido. Tente novamente."]), nl.

promptError(3):- ansi_format([bold,fg(red)], "~w", ["Senha inválida. Tente novamente."]), nl.

promptError(4):- ansi_format([bold,fg(red)], "~w", ["Email/Senha inválido(s). Tente novamente."]), nl.

promptError(5):- ansi_format([bold,fg(red)], "~w", ["Opção inválida. Apenas S/N. Tente novamente."]), nl.

promptError(6):- ansi_format([bold,fg(red)], "~w", ["Nome inválido. Tente novamente."]), nl.

promptError(7):- ansi_format([bold,fg(red)], "~w", ["Email já cadastrado. Tente novamente."]), nl.

promptError(8):- ansi_format([bold,fg(red)], "~w", ["Usuário não cadastrado. Tente novamente."]), nl.

promptError(9):- ansi_format([bold,fg(red)], "~w", ["Sala já cadastrada. Tente novamente."]), nl.

promptError(10):- ansi_format([bold,fg(red)], "~w", ["Sala não encontrada. Tente novamente."]), nl.

promptError(11):- ansi_format([bold,fg(red)], "~w", ["Sala já ocupada neste horário. Tente novamente."]), nl.

promptError(12):- ansi_format([bold,fg(red)], "~w", ["Não foi possível deletar, informações incorretas. Tente novamente."]), nl.

promptError(13):- ansi_format([bold,fg(red)], "~w", ["Não foi possível editar, informações incorretas. Tente novamente."]), nl.

promptError(14):- ansi_format([bold,fg(red)], "~w", ["Apenas uma das opções acima. Tente novamente"]), nl.

promptError(15):- ansi_format([bold,fg(red)], "~w", ["Valor inválido. Tente novamente"]), nl.

promptError(16):- ansi_format([bold,fg(red)], "~w", ["Formato inválido (apenas DD-MM-AAAA). Tente novamente."]), nl.

promptError(17):- ansi_format([bold,fg(red)], "~w", ["Formato inválido (apenas HH:MM). Tente novamente."]), nl.