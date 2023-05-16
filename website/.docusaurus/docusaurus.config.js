export default {
  "title": "GeoLift",
  "tagline": "The Open Source solution from Meta Open Source to calculate Lift at a geo-level.",
  "url": "https://facebookincubator.github.io/",
  "baseUrl": "/GeoLift/",
  "onBrokenLinks": "throw",
  "onBrokenMarkdownLinks": "warn",
  "favicon": "img/favicon.ico",
  "organizationName": "facebookincubator",
  "projectName": "GeoLift",
  "presets": [
    [
      "@docusaurus/preset-classic",
      {
        "docs": {
          "sidebarPath": "/Users/aesquerra/Documents/GitHub/facebookincubator/GeoLift/website/sidebars.js",
          "editUrl": "https://github.com/facebookincubator/GeoLift/"
        },
        "blog": {
          "showReadingTime": true,
          "editUrl": "https://github.com/facebookincubator/GeoLift/"
        },
        "theme": {
          "customCss": "/Users/aesquerra/Documents/GitHub/facebookincubator/GeoLift/website/src/css/custom.css"
        }
      }
    ]
  ],
  "themeConfig": {
    "navbar": {
      "title": "GeoLift",
      "logo": {
        "alt": "GeoLift Logo",
        "src": "img/logo.svg"
      },
      "items": [
        {
          "type": "doc",
          "docId": "intro",
          "position": "left",
          "label": "Documentation"
        },
        {
          "to": "Successes",
          "position": "left",
          "label": "Success Cases"
        },
        {
          "type": "doc",
          "docId": "About",
          "position": "left",
          "label": "About"
        },
        {
          "to": "blog",
          "label": "Blog",
          "position": "left"
        },
        {
          "href": "https://github.com/facebookincubator/GeoLift/",
          "label": "GitHub",
          "position": "right"
        }
      ],
      "hideOnScroll": false
    },
    "footer": {
      "style": "dark",
      "links": [
        {
          "title": "Learn",
          "items": [
            {
              "label": "Documentation",
              "to": "docs/intro"
            },
            {
              "label": "Blueprint Course",
              "href": "https://www.facebookblueprint.com/student/path/253063-geolift-marketing-measurement?sid=cd9a46bc-9685-4af4-8ac3-4482e858fba9&sid_i=5"
            },
            {
              "label": "Success Cases",
              "to": "Successes"
            }
          ]
        },
        {
          "title": "Community",
          "items": [
            {
              "label": "Facebook",
              "href": "https://www.facebook.com/groups/fbgeolift"
            },
            {
              "label": "YouTube",
              "href": "https://www.youtube.com/channel/UCzkuhQFBCHz9TxzUXlNUINg"
            }
          ]
        },
        {
          "title": "More",
          "items": [
            {
              "label": "Blog",
              "to": "blog"
            },
            {
              "label": "GitHub",
              "href": "https://github.com/facebookincubator/GeoLift/"
            }
          ]
        },
        {
          "title": "Legal",
          "items": [
            {
              "label": "Privacy",
              "href": "https://opensource.facebook.com/legal/privacy/"
            },
            {
              "label": "Terms",
              "href": "https://opensource.facebook.com/legal/terms/"
            },
            {
              "label": "Data Policy",
              "href": "https://opensource.facebook.com/legal/data-policy/"
            },
            {
              "label": "Cookie Policy",
              "href": "https://opensource.facebook.com/legal/cookie-policy/"
            }
          ]
        }
      ],
      "logo": {
        "alt": "Facebook Open Source Logo",
        "src": "img/oss_logo.png",
        "href": "https://opensource.facebook.com"
      },
      "copyright": "Copyright © 2022 Facebook, Inc. Built with Docusaurus."
    },
    "colorMode": {
      "defaultMode": "light",
      "disableSwitch": false,
      "respectPrefersColorScheme": false,
      "switchConfig": {
        "darkIcon": "🌜",
        "darkIconStyle": {},
        "lightIcon": "🌞",
        "lightIconStyle": {}
      }
    },
    "docs": {
      "versionPersistence": "localStorage"
    },
    "metadata": [],
    "prism": {
      "additionalLanguages": []
    },
    "hideableSidebar": false,
    "tableOfContents": {
      "minHeadingLevel": 2,
      "maxHeadingLevel": 3
    }
  },
  "baseUrlIssueBanner": true,
  "i18n": {
    "defaultLocale": "en",
    "locales": [
      "en"
    ],
    "localeConfigs": {}
  },
  "onDuplicateRoutes": "warn",
  "staticDirectories": [
    "static"
  ],
  "customFields": {},
  "plugins": [],
  "themes": [],
  "titleDelimiter": "|",
  "noIndex": false
};