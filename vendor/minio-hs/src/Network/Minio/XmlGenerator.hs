--
-- MinIO Haskell SDK, (C) 2017 MinIO, Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

module Network.Minio.XmlGenerator
  ( mkCreateBucketConfig
  , mkCompleteMultipartUploadRequest
  , mkPutNotificationRequest
  , mkSelectRequest
  ) where


import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as H
import qualified Data.Text            as T
import           Text.XML

import           Lib.Prelude

import           Network.Minio.Data


-- | Create a bucketConfig request body XML
mkCreateBucketConfig :: Text -> Region -> ByteString
mkCreateBucketConfig ns location = LBS.toStrict $ renderLBS def bucketConfig
  where
      s3Element n = Element (s3Name ns n) mempty
      root = s3Element "CreateBucketConfiguration"
        [ NodeElement $ s3Element "LocationConstraint"
          [ NodeContent location]
        ]
      bucketConfig = Document (Prologue [] Nothing []) root []

-- | Create a completeMultipartUpload request body XML
mkCompleteMultipartUploadRequest :: [PartTuple] -> ByteString
mkCompleteMultipartUploadRequest partInfo =
  LBS.toStrict $ renderLBS def cmur
  where
    root = Element "CompleteMultipartUpload" mempty $
           map (NodeElement . mkPart) partInfo
    mkPart (n, etag) = Element "Part" mempty
                               [ NodeElement $ Element "PartNumber" mempty
                                 [NodeContent $ T.pack $ show n]
                               , NodeElement $ Element "ETag" mempty
                                 [NodeContent etag]
                               ]
    cmur = Document (Prologue [] Nothing []) root []

-- Simplified XML representation without element attributes.
data XNode = XNode Text [XNode]
           | XLeaf Text Text
  deriving (Eq, Show)

toXML :: Text -> XNode -> ByteString
toXML ns node = LBS.toStrict $ renderLBS def $
  Document (Prologue [] Nothing []) (xmlNode node) []
  where
    xmlNode :: XNode -> Element
    xmlNode (XNode name nodes)   = Element (s3Name ns name) mempty $
                                   map (NodeElement . xmlNode) nodes
    xmlNode (XLeaf name content) = Element (s3Name ns name) mempty
                                   [NodeContent content]

class ToXNode a where
  toXNode :: a -> XNode

instance ToXNode Event where
  toXNode = XLeaf "Event" . show

instance ToXNode Notification where
  toXNode (Notification qc tc lc) = XNode "NotificationConfiguration" $
    map (toXNodesWithArnName "QueueConfiguration" "Queue") qc ++
    map (toXNodesWithArnName "TopicConfiguration" "Topic") tc ++
    map (toXNodesWithArnName "CloudFunctionConfiguration" "CloudFunction") lc

toXNodesWithArnName :: Text -> Text -> NotificationConfig -> XNode
toXNodesWithArnName eltName arnName (NotificationConfig id arn events fRule) =
  XNode eltName $ [XLeaf "Id" id, XLeaf arnName arn] ++ map toXNode events ++
  [toXNode fRule]

instance ToXNode Filter where
  toXNode (Filter (FilterKey (FilterRules rules))) =
    XNode "Filter" [XNode "S3Key" (map getFRXNode rules)]

getFRXNode :: FilterRule -> XNode
getFRXNode (FilterRule n v) = XNode "FilterRule" [ XLeaf "Name" n
                                                 , XLeaf "Value" v
                                                 ]

mkPutNotificationRequest :: Text -> Notification -> ByteString
mkPutNotificationRequest ns = toXML ns . toXNode

mkSelectRequest :: SelectRequest -> ByteString
mkSelectRequest r = LBS.toStrict $ renderLBS def sr
  where
    sr = Document (Prologue [] Nothing []) root []
    root = Element "SelectRequest" mempty $
           [ NodeElement (Element "Expression" mempty
                          [NodeContent $ srExpression r])
           , NodeElement (Element "ExpressionType" mempty
                          [NodeContent $ show $ srExpressionType r])
           , NodeElement (Element "InputSerialization" mempty $
                          inputSerializationNodes $ srInputSerialization r)
           , NodeElement (Element "OutputSerialization" mempty $
                          outputSerializationNodes $ srOutputSerialization r)
           ] ++ maybe [] reqProgElem (srRequestProgressEnabled r)
    reqProgElem enabled = [NodeElement
                           (Element "RequestProgress" mempty
                             [NodeElement
                              (Element "Enabled" mempty
                                [NodeContent
                                 (if enabled then "TRUE" else "FALSE")]
                              )
                             ]
                           )
                          ]
    inputSerializationNodes is = comprTypeNode (isCompressionType is) ++
                                 [NodeElement $ formatNode (isFormatInfo is)]
    comprTypeNode (Just c) = [NodeElement $ Element "CompressionType" mempty
                              [NodeContent $ case c of
                                  CompressionTypeNone  -> "NONE"
                                  CompressionTypeGzip  -> "GZIP"
                                  CompressionTypeBzip2 -> "BZIP2"
                              ]
                             ]
    comprTypeNode Nothing = []

    kvElement (k, v) = Element (Name k Nothing Nothing) mempty [NodeContent v]
    formatNode (InputFormatCSV (CSVProp h)) =
      Element "CSV" mempty
      (map NodeElement $ map kvElement $ H.toList h)
    formatNode (InputFormatJSON p) =
      Element "JSON" mempty
      [NodeElement
        (Element "Type" mempty
          [NodeContent $ case jsonipType p of
              JSONTypeDocument -> "DOCUMENT"
              JSONTypeLines    -> "LINES"
          ]
        )
      ]
    formatNode InputFormatParquet = Element "Parquet" mempty []

    outputSerializationNodes (OutputSerializationJSON j) =
      [NodeElement (Element "JSON" mempty $
                     rdElem $ jsonopRecordDelimiter j)]
    outputSerializationNodes (OutputSerializationCSV (CSVProp h)) =
      [NodeElement $ Element "CSV" mempty
       (map NodeElement $ map kvElement $ H.toList h)]

    rdElem Nothing = []
    rdElem (Just t) = [NodeElement $ Element "RecordDelimiter" mempty
                        [NodeContent t]]
