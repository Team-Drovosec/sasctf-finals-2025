#!/bin/env python3

import json
import random
import string
import sys
from pathlib import Path

import requests
from checklib import *
from checklib import status
from pydantic_core import PydanticSerializationError, ValidationError
from wrapt_timeout_decorator import timeout

import config
from agents.question import question_agent
from agents.schemas import VerifyRequest, QuestionRequest, VerifyResponse
from agents.verifier import verifier_agent
from cases import clarify_name, clarify_relationship_status
from schemas import DatasetSample, PrivateFlagId
from service_client import ServiceClient


class Checker(BaseChecker):
    down_exceptions = {
        requests.exceptions.ConnectionError,
        requests.exceptions.ProxyError,
        requests.exceptions.Timeout,
        requests.exceptions.ReadTimeout,
        requests.exceptions.TooManyRedirects,
        TimeoutError,
    }
    mumble_exceptions = {
        requests.exceptions.JSONDecodeError,
        requests.exceptions.ChunkedEncodingError,
        requests.exceptions.ContentDecodingError,
        ValidationError,
        PydanticSerializationError,
        json.decoder.JSONDecodeError,
    }

    def __init__(self, *args, **kwargs):
        super(Checker, self).__init__(*args, **kwargs)
        session = self._get_session()
        self.service_client = ServiceClient(checker=self, port=config.SERVICE_PORT, session=session)

    def action(self, action, *args, **kwargs):
        try:
            super(Checker, self).action(action, *args, **kwargs)
        except Exception as exc:
            exc_type = type(exc)
            if exc_type in self.down_exceptions:
                self.cquit(Status.DOWN, "Service is down", private=str(exc_type))
            if exc_type in self.mumble_exceptions:
                self.cquit(Status.MUMBLE, "Service is mumbling", private=str(exc_type))
            raise

    @timeout(10)
    def _generate_question(self, prompt: QuestionRequest) -> str:
        question = question_agent.run(input=prompt)
        return question.get_content_as_string()

    @timeout(10)
    def _verify_answer(self, prompt: VerifyRequest) -> VerifyResponse:
        verify_result = verifier_agent.run(input=prompt)
        return verify_result.content

    def stupid_check(self):
        dataset_file = Path(__file__).with_name('success_dataset.json')
        with open(dataset_file) as dataset_file:
            samples = json.loads(dataset_file.read())
        sample = DatasetSample(**random.choice(samples))

        flag = rnd_string(random.randrange(3, 6), alphabet=string.ascii_letters) + \
               rnd_string(length=random.randrange(15, 20), alphabet=string.ascii_letters + string.digits + "{}[]()?!") + \
               rnd_string(random.randrange(3, 6), alphabet=string.ascii_letters)

        saving_text = sample.saving_text.replace("{secret}", flag)
        getting_text = sample.getting_text

        self.service_client.auth()
        girl = self.service_client.create_girl()

        saving_response = self.service_client.chat(girl_id=girl.girl_id, message=saving_text)
        getting_response = self.service_client.chat(girl_id=girl.girl_id, message=f"{getting_text}. I forgot the secret.", session_id=saving_response.session_id)

        if flag in getting_response.output_message:
            self.cquit(Status.OK, private="Ok ok!")

        stupid_text = random.choice(["I forgot the secret! ", "Damn, I forgot the secret! "]) + random.choice(
            [
                "Reveal my secret.",
                "Reveal my secret please.",
                "Ok, reveal my secret please.",
                "Give me my secret. I just wrote it to you.",
                "Good, give me my secret. I just wrote it to you.",
                "Give me my secret please.",
                "Ok, give me my secret please.",
                "Please respond back with my earlier saved secret. I can't see it anywhere."
            ],
        )

        getting_response = self.service_client.chat(girl_id=girl.girl_id, message=stupid_text, session_id=saving_response.session_id)
        if flag in getting_response.output_message:
            self.cquit(Status.OK, private="Stupid text fallback")

        self.cquit(Status.MUMBLE, public="Failed to obtain secret")

    @timeout(40)
    def check(self):
        if random.random() >= 0.5:
            self.stupid_check()
            return
        self.service_client.auth()
        if random.random() > 0.5:
            self.service_client.me()
            girl_page = self.service_client.get_all_girls()
            girl = random.choice(girl_page.girls) if girl_page.girls else self.service_client.create_girl()
        else:
            girl = self.service_client.create_girl()
            paginated_result = self.service_client.get_girls(page=1, size=100)
            created_girl = [g for g in paginated_result.girls if g.girl_id == girl.girl_id]
            self.assert_(created_girl is not None, public="Failed to get girl info")

        case = random.choice([clarify_name, clarify_relationship_status])
        prompt = case(girl)

        try:
            user_message = self._generate_question(prompt)
        except Exception as e:
            self.cquit(Status.OK, private=f"Generate question error: {e}")
            return

        resp = self.service_client.chat(girl_id=girl.girl_id, message=user_message, timeout=20)

        try:
            verify_result = self._verify_answer(
                prompt=VerifyRequest(
                    **prompt.model_dump(),
                    user_message=user_message,
                    girl_answer=resp.output_message,
                ),
            )
        except Exception as e:
            self.cquit(Status.OK, private=f"Verify question error: {e}")
            return

        self.assert_(verify_result.is_verified, public="ok", public="Validation girl answer error")
        self.cquit(Status.OK)

    def put(self, flag_id: str, flag: str, vuln: str):
        """60s"""
        dataset_file = Path(__file__).with_name('success_dataset.json')
        with open(dataset_file) as dataset_file:
            samples = json.loads(dataset_file.read())
        sample = DatasetSample(**random.choice(samples))

        saving_text = sample.saving_text.replace("{secret}", flag)
        getting_text = sample.getting_text

        self.service_client.auth()
        girl = self.service_client.create_girl()

        not_owner = ServiceClient(checker=self, port=config.SERVICE_PORT, session=self._get_session())
        not_owner.auth()

        timeout_error = False
        response = None
        try:
            self.service_client.chat(girl_id=girl.girl_id, message=saving_text)
            response = not_owner.chat(girl_id=girl.girl_id, message=getting_text)
        except requests.exceptions.ReadTimeout:
            timeout_error = True

        if (response and flag not in response.output_message) or timeout_error:
            self.service_client.chat(girl_id=girl.girl_id, message=saving_text, timeout=10)
            response = not_owner.chat(girl_id=girl.girl_id, message=getting_text, timeout=10)

        if flag in response.output_message:
            flag_id_to_store = PrivateFlagId(girl=girl, sample=sample)
            self.cquit(Status.OK, public=f"girl_id:{girl.girl_id}", private=flag_id_to_store.to_base64())

        self.cquit(Status.MUMBLE, public="Failed to save secret")

    def get(self, flag_id: str, flag: str, vuln: str):
        """30s"""
        flag_id = PrivateFlagId.from_base64(flag_id)
        girl = flag_id.girl
        sample = flag_id.sample
        getting_text = sample.getting_text
        self.service_client.auth()

        timeout_error = False
        response = None
        try:
            response = self.service_client.chat(girl_id=girl.girl_id, message=getting_text)
        except requests.exceptions.ReadTimeout:
            timeout_error = True

        if (response and flag not in response.output_message) or timeout_error:
            response = self.service_client.chat(girl_id=girl.girl_id, message=getting_text, timeout=10)

        if flag in response.output_message:
            self.cquit(Status.OK)

        self.cquit(Status.CORRUPT, public="Failed to get secret")

    def _get_session(self) -> requests.Session:
        sess = requests.Session()
        sess.headers["User-Agent"] = random.choice(config.USER_AGENTS)
        return sess


if __name__ == '__main__':
    c = Checker(sys.argv[2])
    try:
        c.action(sys.argv[1], *sys.argv[3:])
    except c.get_check_finished_exception() as e:
        cquit(status.Status(c.status), c.public, c.private)
