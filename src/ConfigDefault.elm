module Config exposing (..)

import Dict exposing (Dict)



-- Backend REST API
apiBaseUrl : String
apiBaseUrl =
    "http://www.imicrobe.us/api/v1"



---- Agave API ----


-- Base URL
agaveBaseUrl : String
agaveBaseUrl =
    "https://agave.iplantc.org"


-- OAuth2 Client ID
oauthClientId : String
oauthClientId =
    ""

-- OAuth2 URL
oauthUrl : String
oauthUrl =
    "https://agave.iplantc.org/authorize"


dataCommonsUrl: String
dataCommonsUrl =
    "http://datacommons.cyverse.org/browse"



---- Plan B ----


-- Base URL
planbBaseUrl : String
planbBaseUrl =
    "https://www.imicrobe.us/plan-b"
