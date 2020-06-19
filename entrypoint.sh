#!/bin/sh


case "$1" in
    deploy)
        echo mycookie > $HOME/.erlang.cookie
        chmod 400 $HOME/.erlang.cookie
        /buildroot/_build/default/rel/erlbank_flex_transactions/bin/erlbank_flex_transactions foreground
        ;;
    test)
        rebar3 eunit
        ;;
    deploy-with-elk)

        if ! [ -x "$(command -v curl)" ]; then
            apk add curl
        fi

        # wait for elastic search
        while true; do # wait for local elastic search
            echo "$(date) waiting elastic search ..."
            curl -s -XGET "elasticsearch-host:9200/_cluster/state"
            if [ "$?" -eq 0 ]; then
                break
            fi
            trials=$((trials-1))
            sleep 10
        done

        # Wait for kibana
        while true; do # wait for local kibana
            echo "$(date) waiting kibana ..."
            curl -s -XGET "kibana-host:5601"
            if [ "$?" -eq 0 ]; then
                break
            fi
            sleep 10
        done

        # Create default index
        echo "Creating kibana index..."
        curl -XPOST -H 'Content-Type: application/json' \
             -H 'kbn-xsrf: anything' \
             'http://kibana-host:5601/api/saved_objects/index-pattern/erlbank-*' \
             '-d{"attributes":{"title":"erlbank-*","timeFieldName":"@timestamp"}}'

        # Wait for logstash
        while true; do # wait for local logstash
            echo "$(date) waiting logstash ..."
            curl -s -XGET "logstash-host:9600"
            if [ "$?" -eq 0 ]; then
                break
            fi
            sleep 10
        done

        sleep 30

        echo mycookie > $HOME/.erlang.cookie
        chmod 400 $HOME/.erlang.cookie
        /buildroot/_build/default/rel/erlbank_flex_transactions/bin/erlbank_flex_transactions foreground
        ;;
    *)
        sh -c $@
        ;;
esac
