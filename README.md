<p id="logo" align="center">
  <img width="30%" src="https://user-images.githubusercontent.com/61962950/110835508-a4a91780-827d-11eb-813a-73bf717c8e23.png" />
</p>
<div id="especificacao">
<p>Neste projeto da disciplina Paradigmas de Linguagens de Programação, desenvolveremos uma aplicação nas linguagens:</p>
<ul>
  <li>Haskell</li>
  <li>Prolog</li>
</ul>

<h2>Problemática</h2>
<p align="justify">A ideia do programa surgiu da necessidade que coordenadores e órgãos administrativos têm de controlar e organizar o uso de salas e/ou ambientes de trabalho de forma holística. Dessa forma, surgiu a ideia de um sistema computacional que possa ser monitorado e editado, atualizando-se instantaneamente para mostrar o estado de ocupação do prédio.</p>

<h2> Objetivo </h2>
<p align="justify">O SIGES é um sistema de gerenciamento e controle que opera sobre as salas e locais de um determinado bloco/prédio para organizar, visualizar e controlar o uso destas pelas pessoas. Com este sistema, é possível reservar salas, visualizar o uso delas (e quem as usa) previamente ou em tempo real, a fim de auxiliar órgãos administrativos/empresariais na organização do seu trabalho e servir como fonte de informação aos usuários sem interromper as atividades correntes.</p>

<h2> Funcionalidades </h2>
<ul>
  <li><h4>Registrar</h4>
    <ol>
      <li>Permitir ao usuário criar uma conta para reservar e cancelar a ocupação de salas;</li>
      <li>Permitir a visualização das salas sem a necessidade de registro (o que garante a rapidez uma melhor interação do usuário com o programa, sem muitos rodeios).</li>
    </ol>
  </li>
  <li><h4>Relatórios de ocupação</h4>
    <ol>
      <li>Responsável pela ocupação;</li>
      <li>Qual a duração da reunião/evento na sala;</li>
      <li>Capacidade de ocupação da sala;</li>
      <li>Motivo, explicação sobre o que acontece na sala (i.e. palestra, prova escrita, reunião da gerência, reforma, etc.), assim usuários podem saber qual sala acontece tal evento sem precisar interromper.</li>
    </ol>
  </li>
  <li><h4>Reservar</h4>
    Ser capaz de ocupar salas (no sistema) e atualizar todo o sistema para que a visualização inclua a nova reserva;
  </li>
  <li><h4>Cancelar</h4>
    <ol>
      <li>Permitir ao usuário remover ocupações de sala em casos de cancelamento ou mudança de planos;</li>
      <li>Permitir ao usuário transferir uma ocupação, sem precisar reinserir todas as informações;</li>
    </ol>
  </li>
  <li><h4>Chatbot</h4>
    <ol>
      <li>Interação orgânica com o usuário para realizar as operações do sistema;</li>
      <li>Identificar erros (sem quebrar o sistema) ao usar o sistema;</li>
      <li>Dar detalhes ao usuário sobre os erros em ii;</li>
      <li>Permitir ao usuário escolher outras opções quando identificar erros como em ii ou digitar opções inválidas;</li>
      <li>Oferecer indicações de sala baseadas em informações providas pelo usuário (tipo de sala, capacidade, tempo de duração, equipamentos);</li>
    </ol>
  </li>
</ul>
</div>

<div id="UMLs">
  <h2>Use cases</h2>
  <img src="https://user-images.githubusercontent.com/62446763/110842773-4d5b7500-8286-11eb-97cc-d0ee61a273b5.png">
  <h2>Diagrama de fluxo do sistema</h2>
  <img src="https://user-images.githubusercontent.com/61962950/111155223-f221cf00-8572-11eb-91af-1dda3922a428.png">
</div>
