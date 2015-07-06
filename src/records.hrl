-record (cmd, {name = undefined, data = undefined}).
-record (ans, {code = 400, body = "Bad Request\n", headers = [{"Content-Type", "text/plain"}]}).

-record (resources, {allocated = undefined, deallocated = []}).