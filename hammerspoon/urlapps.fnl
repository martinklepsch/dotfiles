(local DefaultBrowser "com.brave.Browser")
(local IcebreakerBrowser "com.google.Chrome")
(local Zoom "us.zoom.xos")

(fn init []
  (spoon.SpoonInstall:andUse
    "URLDispatcher"
    {:start true
     :config {:default_handler DefaultBrowser
              :url_patterns [[ "sentry.*icebreaker" IcebreakerBrowser ]
                             [ "github.*gatheround" IcebreakerBrowser ]
                             [ "github.*gatheround" IcebreakerBrowser ]
                             [ "https://.*asana.com"  IcebreakerBrowser ]
                             [ "https://geekbot.com"  IcebreakerBrowser ]
                             [ "tuple.app"  IcebreakerBrowser ]
                             [ "datastudio.google.com" IcebreakerBrowser ]
                             [ "analytics.amplitude.com" IcebreakerBrowser ]
                             [ "getcloudapp.com" IcebreakerBrowser ]
                             [ "loom.com" IcebreakerBrowser ]
                             [ "figma.com" IcebreakerBrowser ]
                             [ "docs.google.com" IcebreakerBrowser ]
                             [ "https?://zoom.us/j/" Zoom ]
                             [ "https?://%w+.zoom.us/j/" Zoom ]]}}))

{:init init}

