module Example.Repro.Container (component) where

import Data.Const
import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Example.Repro.Button1 as Example.Repro.Button1
import Example.Repro.Button2 as Example.Repro.Button2
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type PageComponent = forall m . H.Component HH.HTML (Const Void) Unit Void m

data Page
  = Page1
  | Page2

showPage :: Page -> String
showPage = case _ of
  Page1 -> "Page1"
  Page2 -> "Page2"

togglePage :: Page -> Page
togglePage = case _ of
  Page1 -> Page2
  Page2 -> Page1

pageToPageComponent :: Page -> PageComponent
pageToPageComponent = case _ of
  Page1 -> page1
  Page2 -> page2

page1 :: PageComponent
page1 = Example.Repro.Button1.component

page2 :: PageComponent
page2 = Example.Repro.Button2.component

type State = { page :: Page, checkRerenderButtonEnabled :: Boolean }

data Action
  = TogglePage
  | ToggleCheckRerender

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { page: Page1, checkRerenderButtonEnabled: false }

render :: forall m. State -> H.ComponentHTML Action _ m
render state =
  HH.div_
    [ HH.button
        [ HE.onClick \_ -> Just ToggleCheckRerender
        ]
        [ HH.text $ "Check Rerender button: " <> if state.checkRerenderButtonEnabled then "On" else "Off" ]
    , HH.text $ "Current page: " <> showPage state.page
    , HH.button
        [ HE.onClick \_ -> Just TogglePage
        ]
        [ HH.text "Toggle page" ]
    , HH.slot (SProxy :: SProxy "page") unit (pageToPageComponent state.page) unit absurd
    ]

handleAction ∷ forall o m. Action → H.HalogenM State Action _ o m Unit
handleAction = case _ of
  TogglePage ->
    H.modify_ \st -> st { page = togglePage st.page }
  ToggleCheckRerender ->
    H.modify_ \st -> st { checkRerenderButtonEnabled = not st.checkRerenderButtonEnabled }
