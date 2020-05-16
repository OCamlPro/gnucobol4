*>COPY "./copybooks/Log-File-Select.cpy".
    SELECT Log-File
        ASSIGN DISK "./logs/main.log"
        FILE STATUS IS WS-StatusLOG
        ORGANIZATION LINE SEQUENTIAL
        SHARING ALL.
