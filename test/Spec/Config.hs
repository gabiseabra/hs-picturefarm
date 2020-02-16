module Spec.Config
  ( testConfigWithDefaults
  , testConfig
  )
where

import           Env

testConfig = testConfigWithDefaults
  (Just Config { databaseUrl     = ""
               , port            = 4020
               , cdnCloudName    = "test"
               , cdnUploadPreset = "test"
               , cdnCredentials  = "test"
               }
  )

testConfigWithDefaults = flip loadConfigWithDefaults (Just Test)
