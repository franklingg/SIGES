:- module(errorHandler, [promptError/1]).

:- encoding(utf8).

promptError(0).

promptError(1):- writeln("Opção inválida. Tente novamente.").

promptError(2):- writeln("Email inválido. Tente novamente.").

promptError(3):- writeln("Senha inválida. Tente novamente.").

promptError(4):- writeln("Email/Senha inválido(s). Tente novamente.").

promptError(5):- writeln("Opção inválida. Apenas S/N. Tente novamente.").

promptError(6):- writeln("Nome inválido. Tente novamente.").

promptError(7):- writeln("Email já cadastrado. Tente novamente.").

promptError(8):- writeln("Usuário não cadastrado. Tente novamente.").

promptError(9):- writeln("Sala já cadastrada. Tente novamente.").

promptError(10):- writeln("Sala não encontrada. Tente novamente.").

promptError(11):- writeln("Sala já ocupada neste horário. Tente novamente.").

promptError(12):- writeln("Não foi possível deletar, informações incorretas. Tente novamente.").

promptError(13):- writeln("Não foi possível editar, informações incorretas. Tente novamente.").

promptError(14):- writeln("Apenas uma das opções acima. Tente novamente").

promptError(15):- writeln("Valor inválido. Tente novamente").

promptError(16):- writeln("Formato inválido (apenas DD-MM-AAAA). Tente novamente.").