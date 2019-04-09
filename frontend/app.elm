import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string, map2, map3)
import Debug
import Markdown


-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL


type Model
  = Failure (String)
  | Loading
  | Repos (List Repo)
  | Issues (List Issue)


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getRepos)


-- UPDATE


type Msg
  = ShowRepos
  | ShowIssues (String)
  | GotRepos (Result Http.Error (List Repo))
  | GotIssues (Result Http.Error (List Issue))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShowRepos -> (Loading, getRepos)
    ShowIssues repo -> (Loading, getIssues repo)
    GotRepos result ->
        case result of
            Ok repos -> (Repos repos, Cmd.none)
            Err s -> (Failure (Debug.toString s), Cmd.none)
    GotIssues result ->
        case result of
            Ok issues -> (Issues issues, Cmd.none)
            Err s -> (Failure (Debug.toString s), Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Issue Wanted" ]
    , case model of
        Loading -> text "Loading..."
        Failure s -> text s
        Issues issues -> div [] [ button [onClick ShowRepos] [text "back"], renderIssues issues]
        Repos repos -> renderRepos repos
    ]

renderRepos : List Repo -> Html Msg
renderRepos repos =
    ul [] (List.map (\repo -> li [] [  button [onClick (ShowIssues repo.repoName)] [text "?"],
                                       text ("   "  ++ (repo.repoName)) ]) repos)

renderIssues : List Issue -> Html Msg
renderIssues issues =
    ul [] (List.map (\issue -> li [] [ a [href issue.issueUrl] [text issue.issueName]
                                         , hr [] [Markdown.toHtml [] issue.description]
                                         ]) issues)


-- HTTP


getRepos : Cmd Msg
getRepos =
  Http.get
    { url = "http://localhost:1234/api/repos/"
    , expect = Http.expectJson GotRepos reposDecoder
    }

reposDecoder : Decoder (List Repo)
reposDecoder =
  Json.Decode.list (map2 Repo (field "repoName" string) (field "repoUrl" string))


getIssues : String -> Cmd Msg
getIssues repo =
    Http.get
    { url = "http://localhost:1234/api/getIssues?repo=" ++ repo
    , expect = Http.expectJson GotIssues issuesDecoder
    }

issuesDecoder : Decoder (List Issue)
issuesDecoder =
    Json.Decode.list (map3 Issue (field "issueName" string) (field "issueUrl" string) (field "description" string))


type alias Repo =
    { repoName : String
    , repoUrl : String
    }

type alias Issue =
    { issueName : String
    , issueUrl : String
    , description : String
    }