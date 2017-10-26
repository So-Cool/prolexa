FROM swipl:latest

# Run the image as a non-root user
#RUN adduser -D myuser
USER myuser

ADD basic_site.pl $HOME
ADD alexa_mod.pl $HOME

ENV PORT 4000
EXPOSE 4000

#CMD ["swipl", "-f", "basic_site.pl", "-g", "server(5000)."] 
#CMD ["swipl", "basic_site.pl", "--user=daemon", "--no-fork", "--port=$PORT"]
CMD swipl basic_site.pl --no-fork --port=$PORT
