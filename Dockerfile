FROM swipl:7.6.0

# Run the image as a non-root user
RUN useradd -m -s /bin/sh myuser
USER myuser
WORKDIR /home/myuser

ADD basic_site.pl $HOME
ADD prolexa.pl $HOME
ADD grammar.pl $HOME
ADD qa_engine.pl $HOME
ADD library.pl $HOME
ADD aux_swi.pl $HOME

ENV PORT 4000
EXPOSE 4000

#CMD ["swipl", "-f", "basic_site.pl", "-g", "server(5000)."] 
#CMD ["swipl", "basic_site.pl", "--user=daemon", "--no-fork", "--port=$PORT"]
CMD swipl basic_site.pl --no-fork --port=$PORT
