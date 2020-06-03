module Config exposing (..)



alertBannerText : Maybe String
alertBannerText =
    Nothing


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


dataCommonsUrl : String
dataCommonsUrl =
    "http://datacommons.cyverse.org/browse"


-- Remove these admin users from Share view in File Browser
filteredUsers : List String
filteredUsers =
    [ "dooley", "vaughn", "rodsadmin", "jstubbs", "jfonner", "eriksf", "QuickShare"
    , "admin2", "admin_proxy", "agave", "bisque-adm", "de-irods", "has_admin", "ibp-proxy"
    , "ipc_admin", "ipcservices", "proxy-de-tools", "uk_admin", "uportal_admin2", "terraref_admin"
    , "avra_admin", "tacc_admin"
    ]



---- Plan B ----


-- Base URL
planbBaseUrl : String
planbBaseUrl =
    "https://www.imicrobe.us/plan-b"



---- ORCID API ----


-- OAuth2 Client ID
orcidClientId : String
orcidClientId =
    ""


-- OAuth2 URL
orcidOAuthUrl : String
orcidOAuthUrl =
    "https://sandbox.orcid.org/oauth/authorize"


-- Base URL
orcidBaseUrl : String
orcidBaseUrl =
    "https://api.sandbox.orcid.org/v2.0"