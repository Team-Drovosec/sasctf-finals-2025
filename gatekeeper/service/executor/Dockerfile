# syntax=docker/dockerfile:1
FROM python:3.12-slim

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

# Create user/group first (Debian syntax)
RUN groupadd --gid 1000 executor \
    && useradd --uid 1000 --gid 1000 --create-home \
    --shell /usr/sbin/nologin executor

WORKDIR /opt/gatekeeper-executor

# # install pwndbg
# RUN apt update && apt install -y git curl wget xz-utils \
#     && curl -qsL 'https://install.pwndbg.re' | sh -s -- -t pwndbg-gdb

# RUN apt install -y gdbserver

# Install deps as root (system site-packages)
COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

# Copy app and fix ownership so the non-root user can read/write if needed
COPY . .
RUN chown -R executor:executor /opt/gatekeeper-executor

RUN mkdir -p /var/gatekeeper/executor && chown -R executor:executor /var/gatekeeper/executor \
    && touch /var/gatekeeper/executor/.keep

EXPOSE 2047

# Drop privileges after installs are done
USER executor

CMD ["gunicorn", "-b", "0.0.0.0:2047", "main:app"]