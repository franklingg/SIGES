{-|
Module      : Datatypes
Description : Módulo contendo os diferentes módulos próprios do sistema SIGES.
-}
{-# LANGUAGE DeriveGeneric #-}
module Datatypes where

import Data.ByteString
import Data.Text
import GHC.Generics
import Data.Time


-- | Versão resumida de usuário do sistema. Mantém dados essenciais, mas omite dados sensíveis que não serão necessários.
data User = User {
    nameUser :: String  -- ^ O nome do Usuário.
   ,emailUser :: String -- ^ O e-mail do usuário.
   ,isAdminUser :: Bool -- ^ Indicação se o usuário é ou não um administrador.
} deriving(Eq, Generic)

-- | Versão completa do usuário do sistema, a ser usada quando se precisa de todas as informações do usuário.
data UserFull = UserFull {
    name :: String             -- ^ O nome do usuário.
   ,email :: String            -- ^ O e-mail do usuário.
   ,password :: Text           -- ^ A senha (criptografada) do usuário.
   ,registrationDate :: String -- ^ A data em que o usuário se registrou.
   ,isAdmin :: Bool            -- ^ Indicação se o usuário é administrador.
} deriving(Eq, Show, Generic)


-- | Define o tipo ErrorLog como uma String.
type ErrorLog = String

-- | As diferentes telas do sistema.
data Screen = ExitScreen              -- ^ A tela de saída do sistema.
            | FirstScreen             -- ^ A tela inicial, quando nenhum administrador foi cadastrado ainda.
            | StartScreen             -- ^ A tela inicial do sistema quando já existem usuários cadastrados.
            | LoginScreen             -- ^ A tela de Login.
            | LoggedScreen            -- ^ A tela após um usuário logar-se no sistema.
            | AdminScreen             -- ^ A tela exibida após um administrador fazer login no sistema.
            | InsertRoomScreen        -- ^ A tela de cadastrar uma nova sala no sistema
            | SignUpScreen            -- ^ A tela de cadastro do usuário.
            | RemoveUserScreen        -- ^ A tela de remoção de um usuário.
            | ViewUserScreen          -- ^ A tela de visualização de salas específica de um usuário.
            | ViewScreen              -- ^ A tela com opções de visualização de salas.
            | ViewRoomScreen          -- ^ A tela de visualização de uma sala específica.
            | ViewFilterScreen        -- ^ A tela de busca de salas por filtros.
            | ReportRoomScreen        -- ^ A tela de exibição de relatório de ocupação para uma sala específica.
            | ReportDayScreen         -- ^ A tela de exibição de relatório de ocupação para todas as salas em um determinado dia.
            | CreateReservationScreen -- ^ A tela para se fazer uma nova reserva em uma sala.
            | EditReservationScreen   -- ^ A tela para se editar uma reserva previamente feita.
            | RemoveReservationScreen -- ^ A tela para se remover uma reserva previamente feita.
            deriving (Eq)

-- | Uma sala do sistema SIGES.
data Room = Room {
    code :: String            -- ^ O código da sala.
   ,schedule :: [Reservation] -- ^ A agenda onde são armazenadas todas as reservas ativas para esta sala.
   ,resources :: [Resource]   -- ^ A lista de recursos oferecidos pela sala.
   ,capacity :: Int           -- ^ A quantidade de pessoas suportada pela sala.
   ,localization :: String    -- ^ A Localização da Sala.
   ,category :: RoomCategory  -- ^ A categoria da sala.

} deriving (Eq, Generic)

-- | As reservas de salas do sistema.
data Reservation = Reservation {
    requester :: String     -- ^ O responsável pela reserva.
   ,description :: String   -- ^ A descrição do evento da reserva.
   ,startTime :: LocalTime  -- ^ O tempo de início do evento.
   ,finishTime :: LocalTime -- ^ O tempo de finalização do evento.

} deriving (Eq, Generic)

-- | As categorias de sala oferecidas pelo sistema.
data RoomCategory = Laboratory -- ^ Sala que funciona como um laboratório.
                  | Auditorium -- ^ Sala que funciona como um auditório.
                  | Classroom  -- ^ Sala que funciona como uma sala de aula.
                  | Office     -- ^ Sala que funciona como um escritório.
                  | Warehouse  -- ^ Sala que funciona como um depósito.
                  deriving (Eq, Generic)

-- | Os tipos de recursos existentes no sistema.
data ResourceKind = Projector      -- ^ Projetor ou Datashow.
                  | Microscope     -- ^ Microscópio.
                  | Desk           -- ^ Escrivaninha.
                  | Computer       -- ^ Computador.
                  | Board          -- ^ Quadro negro ou branco.
                  | AirConditioner -- ^ Ar condicionado.
              deriving (Eq, Generic)

-- | Os recursos oferecidos pelas salas do sistema.
data Resource = Resource {
    resourceKind :: ResourceKind -- ^ O tipo do recurso.
   ,resourceQuantity :: Int      -- ^ A quantidade do recurso que a sala oferece.
}deriving (Eq, Generic)

