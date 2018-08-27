Authentication: identify a user

Authorization: constrain accessibility (CRUD) of resources (information) to the user

Session: State of current conversation with the user
- Cache (mid-term memory)
  - for future request: non-persistent/volatile/short-term user info e.g. session cookie
  - for future response: serve repeated or similar requests without database (long-term memory) lookup

Routing: locate the requested resource
- Read: may gather pieces from different places then transform and compose them to form a response if not directly available somewhere
- Update

