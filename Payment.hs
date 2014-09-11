{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module PaymentProtocol where

import Data.Maybe
import Data.ProtocolBuffers
import Data.Text
import Data.ByteString
import Data.Word
import GHC.Generics

defaultOptional :: (HasField a, FieldType a ~ Maybe b) => b -> a -> a
defaultOptional b = putField . Just . fromMaybe b . getField

data Output = Output
    { amount :: Optional 1 (Value Word64)     -- amount is integer-number-of-satoshis
    , script :: Required 2 (Value ByteString) -- usually one of the standard Script forms
    } deriving (Generic, Show)
instance Encode Output
instance Decode Output
defaultOutput :: Output -> Output
defaultOutput output = output { amount = defaultOptional 0 $ amount output }

data PaymentDetails = PaymentDetails
    { network         :: Optional 1 (Value Text)       -- "main" or "test"
    , outputs         :: Repeated 2 (Message Output)   -- Where payment should be sent
    , time            :: Required 3 (Value Word64)     -- Timestamp; when payment request created
    , expires         :: Optional 4 (Value Word64)     -- Timestamp; when this request should be considered invalid
    , memoPD          :: Optional 5 (Value Text)       -- Human-readable description of request for the customer
    , payment_url     :: Optional 6 (Value Text)       -- URL to send Payment and get PaymentACK
    , merchant_dataPD :: Optional 7 (Value ByteString) -- Arbitrary data to include in the Payment message
    } deriving (Generic, Show)
instance Encode PaymentDetails
instance Decode PaymentDetails
defaultPaymentDetails :: PaymentDetails -> PaymentDetails
defaultPaymentDetails paymentDetails = paymentDetails { network = defaultOptional "main" $ network paymentDetails }

data PaymentRequest = PaymentRequest
    { payment_details_version    :: Optional 1 (Value Word32)
    , pki_type                   :: Optional 2 (Value Text)       -- none / x509+sha256 / x509+sha1
    , pki_data                   :: Optional 3 (Value ByteString) -- depends on pki_type
    , serialized_payment_details :: Required 4 (Value ByteString) -- PaymentDetails
    , signature                  :: Optional 5 (Value ByteString) -- pki-dependent signature
    } deriving (Generic, Show)
instance Encode PaymentRequest
instance Decode PaymentRequest
defaultPaymentRequest :: PaymentRequest -> PaymentRequest
defaultPaymentRequest paymentRequest = paymentRequest
    { payment_details_version = defaultOptional 1 $ payment_details_version paymentRequest
    , pki_type                = defaultOptional "none" $ pki_type paymentRequest
    }

data X509Certificates = X509Certificates
    { certificate :: Repeated 1 (Value ByteString) -- DER-encoded X.509 certificate chain
    } deriving (Generic, Show)
instance Encode X509Certificates
instance Decode X509Certificates

data Payment = Payment
    { merchant_dataP :: Optional 1 (Value ByteString) -- From PaymentDetails.merchant_data
    , transactions   :: Repeated 2 (Value ByteString) -- Signed transactions that satisfy PaymentDetails.outputs
    , refund_to      :: Repeated 3 (Message Output)   -- Where to send refunds, if a refund is necessary
    , memoP          :: Optional 4 (Value Text)       -- Human-readable message for the merchant
    } deriving (Generic, Show)
instance Encode Payment
instance Decode Payment

data PaymentACK = PaymentACK
    { payment :: Required 1 (Message Payment) -- Payment message that triggered this ACK
    , memoACK :: Optional 2 (Value Text)      -- human-readable message for customer
    } deriving (Generic, Show)
instance Encode PaymentACK
instance Decode PaymentACK