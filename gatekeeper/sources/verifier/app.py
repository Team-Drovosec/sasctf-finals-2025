"""
@Author: madrat
Application entrypoint for the Gatekeeper verifier service.
"""

import logging

logging.basicConfig(
    level=logging.DEBUG,
    format="[%(asctime)s] (%(levelname)s) %(name)s: %(message)s",
    datefmt="%H:%M:%S",
)


from verifier_app import create_app

app = create_app()


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=2046, debug=False)
