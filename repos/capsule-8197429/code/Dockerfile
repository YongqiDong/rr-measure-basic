FROM --platform=$BUILDPLATFORM python:3.12-slim
ARG TARGETPLATFORM
ARG BUILDPLATFORM

RUN apt-get update && \
    apt-get install make pipx -y && \
    pipx install poetry==1.7.1

ENV PATH="${PATH}:/root/.local/bin"

WORKDIR /app

COPY poetry.lock pyproject.toml /app

RUN poetry install

ADD src /app/src

VOLUME /app/src/figures

VOLUME /data

ENV PYTHONUNBUFFERED=1

WORKDIR /app/src

CMD ["make"]
