let establish_server server_fun sockaddr =
    match fork() with
       0 -> if fork() <> 0 then sys_exit 0;
            server_fun inchan outchan;

