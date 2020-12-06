-define(LISTENER_NAME, dynamodbtask_service).
-define(PORT, 5555).
-define(TRANSPORT, ranch_tcp).
-define(MAX_CONNECTIONS, infinity).
-define(ACCEPTORS_COUNT, 30).

%% DynamoDB config
-define(TABLE_NAME, <<"dynamostore">>).
-define(ALIAS_NAME, <<"alias/dynamokey">>).
-define(KEY_FIELD, <<"dkey">>).
-define(VALUE_FIELD_1, <<"dval">>).
-define(VALUE_FIELD_2, <<"dtag">>).
-define(READ, 50).
-define(WRITE, 50).

%% AWS config
-define(ACCESS_KEY_ID, "******").
-define(SECRET_ACCCESS_KEY, "******").

%%KMS config
-define(IV, << 0:96 >>).