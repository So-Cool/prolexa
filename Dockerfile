FROM swipl:8.0.3

# Run the image as a non-root user
RUN useradd -m -s /bin/sh myuser
USER myuser
WORKDIR /home/myuser

ADD prolexa/prolog/basic_site.pl $HOME
ADD prolexa/prolog/prolexa.pl $HOME
ADD prolexa/prolog/prolexa_grammar.pl $HOME
ADD prolexa/prolog/prolexa_engine.pl $HOME
ADD prolexa/prolog/library.pl $HOME
ADD prolexa/prolog/aux_swi.pl $HOME

ENV PORT 4000
EXPOSE 4000

#CMD ["swipl", "-f", "basic_site.pl", "-g", "server(5000)."] 
#CMD ["swipl", "basic_site.pl", "--user=daemon", "--no-fork", "--port=$PORT"]
CMD swipl basic_site.pl --no-fork --port=$PORT
